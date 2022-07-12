rm(list = ls())
graphics.off()

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(hflights)
library(lubridate)

df <- hflights

## Use dplyr and tidyr and try to answer the following questions:

# Exercise 1
# 1. How many rows and columns are in table hflights?
nrow(df); ncol(df)
# rows: 227496
# columns

# 2. How many different carriers are listed in the table? Print a table with 
# distinct carrier names
df %>% 
  select(UniqueCarrier) %>% 
  distinct() %>% nrow()

## there are 15 unique carriers


# 3. Which and how many airports were involved? Consider both origin and destination 
# airports
df %>% 
  select(Origin, Dest) %>% 
  # use pivot longer to see
  pivot_longer(cols = everything(),
               names_to = "origins/destinations",
               values_to = "airport") %>% 
  distinct(airport) %>% 
  arrange(airport)

# 4. How many flights were cancelled?
df %>% 
  filter(Cancelled == 1) %>% 
  nrow()

## number of cancelled flights: 2973

# Exercise 2:
# 1. Produce a table where stats for each carrier is shown: Num flights per carrier
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(flights = n())
  ungroup()
# 2. Produce a table where stats for each carrier is shown: total distance flown in miles
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`total distance flown (miles)` = sum(Distance)) %>% 
  ungroup()

# 3. Produce a table where stats for each carrier is shown: total actual elapsed time in hours
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`Actual Elapsed Time (hours)` = round(sum(ActualElapsedTime, na.rm = T) / 60, 1)) %>% 
  ungroup()

# 4. Produce a table where stats for each carrier is shown: total air time in hours
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`Total Airtime (Hours)` = round(sum(AirTime, na.rm = T) / 60, 1)) %>% 
  ungroup()

# 5. Produce a table where stats for each carrier is shown: mean distance per flight 
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`Mean Distance per Flight (miles)` = mean(Distance)) %>% 
  ungroup()
# 6. Produce a table where stats for each carrier is shown: mean actual elapsed time in hours per flight
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`Mean Actual Elapsed Time (hours)` = round(mean(ActualElapsedTime, na.rm = T) / 60, 1)) %>% 
  ungroup()

# 7. Produce a table where stats for each carrier is shown: mean air time in hours per flight
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`Mean Air Time (hours)` = round(mean(AirTime, na.rm = T) / 60, 1)) %>% 
  ungroup()

## Lets put this all together into one table real quick...
carrier_stats <- df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`Number of Flights` = n(),
            `Total Distance Flown (miles)` = sum(Distance),
            `Mean Distance (miles)` = mean(Distance),
            `Total Actual Elapsed Time (hours)` = round(sum(ActualElapsedTime, na.rm = T) / 60, 1), 
            `Mean Actual Elapsed Time (hours)` = round(mean(ActualElapsedTime, na.rm = T) / 60, 1),
            `Total Air Time (hours)` = round(sum(AirTime, na.rm = T) / 60, 1),
            `Mean Air Time (hours)` = round(mean(AirTime, na.rm = T) / 60, 1)
            ) %>% 
  ungroup()

# Second, calculate the percentage of total distance flown by the top 3 performing carriers versus 
# total distance flown by remaining carriers

# 8. first, rank carriers by total distance flown
carrier_stats %>% 
  select(UniqueCarrier,
         distance = `Total Distance Flown (miles)`) %>% 
  arrange(desc(distance)) %>% 
  mutate(rank = row_number())
# 9. top 3 performers are in one group, remaining carriers are in second group
carrier_stats %>% 
  select(UniqueCarrier,
         distance = `Total Distance Flown (miles)`) %>% 
  arrange(desc(distance)) %>% 
  mutate(rank = row_number(),
         group = case_when(rank <= 3 ~ 'Top Performer',
                           TRUE ~ 'The Rest'))
# 10. for each group, calculate the total distance flown
carrier_stats %>% 
  select(UniqueCarrier, 
         distance = `Total Distance Flown (miles)`) %>% 
  arrange(desc(distance)) %>% 
  mutate(rank = row_number(),
         group = case_when(rank <= 3 ~ 'Top Performer',
                           TRUE ~ 'The Rest')) %>% 
  group_by(group) %>% 
  summarise(carriers= n(),
            distance = sum(distance)) %>% 
  ungroup() %>% 
  arrange(desc(distance))
# 11. for each group, calculate percentage: (total distance per group) / total distance all carriers
carrier_stats %>% 
  select(UniqueCarrier,
         distance = `Total Distance Flown (miles)`) %>% 
  arrange(desc(distance)) %>% 
  mutate(rank = row_number(),
         group = case_when(rank <= 3 ~ 'Top Performer',
                           TRUE ~ 'The Rest')) %>% 
  group_by(group) %>% 
  summarise(carriers = n(),
            distance = sum(distance)) %>% 
  ungroup() %>% 
  mutate(`Distance %` = distance / sum(distance) * 100) %>% 
  arrange(desc(distance))

# Exercise 3
# Modify your main flight table:
df <- hflights
# 1. Create Date column by uniting columns: year, month, dayofmonth
df %>% 
  unite(col = 'Date',
        Year, Month, DayofMonth,
        sep = '/')
# 2. when uniting columns do not lose source columns (mutate each column - 
# with slightly different name, before unite operation is executed)
df %>% 
  mutate(src_Year = Year,
         src_Month = Month,
         src_DayofMonth = DayofMonth) %>% 
  unite(col = 'Date',
        Year, Month, DayofMonth, sep = "/")
# 3. you will need to parse date column after unite operation
df %>% 
  mutate(year_ = Year,
         month_ = Month,
         dayofmonth = DayofMonth) %>% 
  mutate_at(.vars = c("Month", "DayofMonth"),
            .funs = str_pad, 2, "left", "0") %>% 
  unite(col = "date",
        Year, Month, DayofMonth, sep = '-') %>% 
  mutate(date = ymd(date))
# 4. also, you should add leading zeros to month and day of month before date 
# is created.
df %>% 
  mutate(src_Year = Year,
         src_Month = Month, 
         src_DayofMonth = DayofMonth) %>% 
  mutate_at(.vars = c("Month", "DayofMonth"),
            .funs = str_pad, 2, "left", "0") %>% 
  unite(col = "Date",
        Year, Month, DayofMonth, sep = "/")
# create columns quarter and week.
df %>% 
  mutate(src_Year = Year, src_Month = Month, src_DayofMonth = DayofMonth) %>% 
  mutate_at(.vars = c("Month", "DayofMonth"),
            .funs = str_pad, 2, "left", "0") %>% 
  unite(col = "Date",
        Year, Month, DayofMonth, sep = '-') %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(quarter = quarter(Date),
         week = isoweek(Date))
## Putting this all together 
df <- df %>% 
  mutate(year = Year, month = Month, day = DayofMonth) %>% 
  mutate_at(.vars = c("Month", "DayofMonth"),
            .funs = str_pad, 2, "left", "0") %>% 
  unite(col = "Date",
        Year, Month, DayofMonth, sep = '-') %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(quarter = quarter(Date),
         week = isoweek(Date)) %>% 
  select(Date, year, month, day, quarter, week, DayOfWeek, everything())

# Is Total number of flights increasing or decreasing quarterly?
df %>% 
  group_by(quarter) %>% 
  summarise(flights = n()) %>% 
  ungroup() %>% 
  mutate(`delta flights` = flights - lag(flights, 1),
         quarter = as.factor(quarter)) %>% 
  ggplot(aes(x = quarter, y = `delta flights`)) + 
  geom_col()

# Is total distance increasing or decreasing monthly?
df %>% 
  group_by(month) %>% 
  summarise(distance = sum(Distance)) %>% 
  ungroup() %>% 
  mutate(`delta distance` = distance - lag(distance, 1),
         month = as.factor(month)) %>% 
  ggplot(aes(x = month, y = `delta distance`)) + 
  geom_col()
  
# Exercise 4
# The idea for the last exercise is another data wrangling task, where you will have to use
# technique called "pivoting". Build a table, that will resemble a heat map by:
# 1. for each carrier and month, calculate total number of flights
df %>% 
  select(carrier = UniqueCarrier,
         month) %>% 
  group_by(carrier, month) %>% 
  summarise(flights = n()) %>% 
  ungroup()

# then normalize total number of flights
df %>% 
  select(carrier = UniqueCarrier,
         month) %>% 
  group_by(carrier, month) %>% 
  summarise(flights = n()) %>% 
  ungroup() %>% 
  mutate(`max flights` = max(flights),
         `flights normalized` = flights / `max flights`)

# pivot the table from long to wide
df %>% 
  select(carrier = UniqueCarrier,
         month) %>% 
  group_by(carrier, month) %>% 
  summarise(flights = n()) %>% 
  ungroup() %>% 
  mutate(`max flights` = max(flights),
         `normalized flights` = flights / `max flights`) %>% 
  select(carrier, month, 
         `flights normalized` = `normalized flights`) %>% 
  pivot_wider(names_from = month,
              values_from = `flights normalized`,
              values_fill = 0)

# so each row is represented with carrier and each column is representd with month, 
# normalized total number of flights are valuies in table cells
# Create the heat map
df %>% 
  select(carrier = UniqueCarrier,
         month) %>% 
  group_by(carrier, month) %>% 
  summarise(flights = n()) %>% 
  ungroup() %>% 
  mutate(`max flights` = max(flights),
         `normalized flights` = flights / `max flights`) %>% 
  select(carrier, month, 
         `flights normalized` = `normalized flights`) %>% 
  mutate(month = as.factor(month)) %>% 
  ggplot(aes(x = carrier,
             y = month,
             fill = `flights normalized`)) +
  geom_tile() + 
  scale_fill_viridis_c(option = "magma") + 
  theme_minimal()
