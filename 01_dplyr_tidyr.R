# 1 tidyverse essentials: 

# first we can remove everything prior
rm(list = ls())

# close any graphics that might have been open
graphics.off()

# install the packages we need
# install.packages("tidyverse")
# install.packages("dplyr")
# intsall.packages("tidyr")
# install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)

# inspect data
help("mpg")
df <- mpg
View(df)
print(df)
str(df)   # see the types of the tibble
nrow(df); ncol(df)

# Manipulate our variables (columns)

# select() - column selection
## extract: manufacturer, model, year
select(df, manufacturer, model, year)
df.car.info <- select(df, manufacturer, model, year)
View(df.car.info)

## extract columns that begin with the letter 'm'
select(df, starts_with(match = "m"))

## extract columns that contain the letter 'r'
select(df, contains('r'))

## extract columns that end with the letter 'y'
select(df, ends_with('y'))

## extract columns by column index, first three
select(df, 1:3)

## what about the 2, 5, and 7th?
select(df, c(2, 5, 7))

## last three
select(df, (ncol(df) - 2) : ncol(df))



# Rename() - rename column

## rename "manufacturer" and "model"
df1 <- rename(df, 
              mnfc = manufacturer,
              mod = model
              )


# select columns and rename in one call
df2 <- select(df, 
         mnfc = manufacturer, 
         mod = model,
         everything())

print(df2)

# mutate() - create new variable

## create variable which is the average between city and highway mpg
df <- mutate(df, `average miles per gallon` = (cty + hwy) / 2)
View(df)

## "car" "cyl" / "trans"
df <- mutate(df, 
             car = paste(manufacturer, model, sep = " "),
             `cyl / trans` = paste(cyl, " cylinders / ", trans, " transmission", seo = "")
             )


View(df)


# transmutate() - create new variable and drop other variables
transmute(df,
          `average miles per gallon` = (cty + hwy) / 2
          )

transmute(df,
          car = paste(manufacturer, model, sep = ' '),
          `cyl / trans` = paste(cyl, " cylinders / ", trans, " transmission", sep = "")
          )

?base::Logic
?Comparison

# reset dataset
rm(df, df.car.info, df1, df2, df3)
df <- mpg

# Manipulate cases (rows)
## filter() - filter rows by condition
## filter where manufacturer is 'audi'
audi <- filter(df, manufacturer == 'audi')

filter(df, manufacturer == 'audi', year == 1999)
# which is the same as 
filter(df, manufacturer == 'audi' & year == 1999)


# where manufacturer is 'audi' or 'dodge' 
df1 <- filter(df, manufacturer == 'audi' | manufacturer == 'dodge')

## for cases with greater than or equal value
filter(df, hwy >= 30)

## for the NOT operator
filter(df, year != 1999)

# slice() - extract rows by position
## extract the first five rows
slice(df, 1:5)

## extract rows 22 to 30
slice(df, 22:30)

## extract last 10 rows
slice(df, (nrow(df) - 9): nrow(df))
slice(df, c(1, 3, 5, 7, 9))

trucks <- filter(df, class == "pickup")


# arrange() - sort rows

## sort rows by year in ascending order
arrange(df, year)

## now in descending order
arrange(df, desc(year))

## sort rows by year in ascending, number of cyl, and displ
df_sort <- arrange(df, year, cyl, displ)

# distinct() - unique rows

## we will create a small example to work with
df.example <- data.frame(id = 1:3,
                         name = c("John", "Max", "Julia")
                         )

df.example <- bind_rows(df.example, slice(df.example, 2))
df.example <- arrange(df.example, id)

## remove duplicate row
distinct(df.example)

## back to mpg, but let's first add some duplicates
df.dupl <- select(df, manufacturer, model)

## now lets keep only the original rows
df.nodup <- distinct(df.dupl)

# sample rows

# sample_n() - randomly select different rows from table
set.seed(567)    # this is just so it pulls the same results next run.

## 10 randomly selected rows without replacement
### this means a row can only be selected once

sample_n(df, size = 10, replace = FALSE)

## 10 randomly selected rows WITH replacement
sample_n(df, size = 10, replace = T)

# sample_frac() - randomly select a fraction of rows

## randomly select 10% of rows from table
sample_frac(df, size = 0.1, replace = F)

# summerise() - aaply summery function on our table and create summeries

## calculate average hwy 
summarise(df, `mean hwy consumption` = mean(hwy))

## count table rows and count distinct car models
summarise(df,
          `rows` = n(),
          `num models` = n_distinct(model))

## calculate min / max value for hwy and cty
summarise(df,
          `min hwy` = min(hwy),
          `max hwy` = max(hwy),
          `min cty` = min(cty),
          `max cty` = max(cty))

# group_by() - group cases ousing 1+ grouping variables
df
group_by(df, manufacturer)

## combine summerise and group_by - summery stats for groups
## count num of cars per each manufacturer

summarise(group_by(df, manufacturer),
          cars = n())

## calculate min/max for hwy and cty, group by model
df.group_model <- group_by(df, model)

summarise(df.group_model,
          `min hwy` = min(hwy),
          `max hwy` = max(hwy),
          `min cty` = min(cty),
          `max cty` = max(cty))

## count() - count rows for group variables

## count number of table rows
count(df)

## count number of rows for cars per model
count(df, model)

# pip operator %>% 
## chain dpylr functions using %>%
## every step is executed in a single 

## count number of cars where manufacture is audi

df %>% 
  filter(manufacturer == 'audi') %>% 
  count()

## filter rows for manufacturer dodge or Chevrolet
## select only columns manufacturer, model, year, and class
df %>% 
  filter(manufacturer == 'dodge' | manufacturer == 'chevrolet') %>% 
  select(manufacturer, model, year, class) %>% 
  View()

## calculate average hwy and count number of cars
## for each manufacturer and model, class, and transmission
## also filter results where average hwy > 30 and show results in desc
df %>% 
  group_by(manufacturer, model, class, trans) %>% 
  summarise(`mean hwy` = mean(hwy),
            cars = n()) %>% 
  ungroup() %>% 
  filter(`mean hwy` > 30) %>% 
  arrange(desc(`mean hwy`))


# pivoting - convert table from long to wide format, and vice versa

## create a simple table
table.long <- data.frame(id = 1:6,
                         type = c('a', 'b', 'a', 'c', 'c', 'a'),
                         count = c(20, 50, 45, 15, 12, 5))

table.long

# pivot_wider() - covert long table to wide
## convert table to wide format.
## Each 'type' is written in it's own column
table.wide <- pivot_wider(table.long,
                          names_from = type,
                          values_from = count)

table.wide

## pivot_longer() - convert wide table to longer

## convert table to long format

table.long1 <- pivot_longer(table.wide,
                            cols = c('a', 'b', 'c'),
                            names_to = 'type',
                            values_to = 'count',
                            values_drop_na = T)
table.long1
table.long


## Now let's pivot the car data

## first filter the rows where manufacturer is jeep or land rover or hyundai
## select model, trans, hwy
## calculate average hwy for each model and trans.
## convert to long table format

df.long <- df %>% 
              filter(manufacturer %in% c('jeep', 'land rover', 'hyundai')) %>% 
              select(model, trans, hwy) %>% 
              group_by(model, trans) %>% 
              summarise(`mean hwy` = mean(hwy)) %>% 
              ungroup()

View(df.long)

## now convert from long to wide format where trans is transformed into columns

df.wide <- df.long %>% 
  pivot_wider(names_from = trans, values_from = `mean hwy`)

df.wide  

df.long1 <- df.wide %>% 
  pivot_longer(-model, # exclude column model and use everything else
               names_to = "trans",
               values_to = "mean hwy",
               values_drop_na = T
               )
df.long1

# separating & uniting columns

## create some date data (generate dates for 1 year)
dates <- seq.Date(from = as.Date("2022-01-01"),
                  to = as.Date("2022-12-31"),
                  by = "day")

table <- data.frame(date = dates)

table %>% head()
table %>% tail()

# separate() - split one column into multiple columns


## split date into year, month, and day
## remove leading zeros
## sort columns

table.sep <- table %>% 
  separate(data = .,
           col = date,
           into = c("year", "month", "day_of_month"),
           sep = "-") %>% 
  mutate(month = as.numeric(month),
         day_of_month = as.numeric(day_of_month)) %>% 
  arrange(year, month, day_of_month)

# We can also do it like so

table.sep2 <- table %>% 
  separate(data = .,
           col = date,
           into = c("year", "month", "day_of_month"),
           sep = "-") %>% 
  mutate_at(.tbl = .,
            .vars = c("month", "day_of_month"),
            .funs = as.numeric) %>% 
  arrange(year, month, day_of_month)

# unite() - combine multiple columns into single column
## add leading zeros for month, day_of_month
library(stringr)

## create one date column.
### merge year, month, day of month

table.unite <- table.sep %>% 
  # add leading zeros
  mutate(month = str_pad(month, width = 2, side = "left", pad = "0")) %>% 
  mutate(day_of_month = str_pad(day_of_month, width = 2, side = "left", pad = "0")) %>% 
  unite(data = .,
        col = "date",
        month, day_of_month, year,
        sep = "/")

table.unite2 <- table.sep %>% 
  mutate_at(.tbl = .,
            .vars = c("month", "day_of_month"),
            .funs = str_pad, 2, "left", "0") %>% 
  unite(data = .,
        col = 'date',
        month, day_of_month, year,
        sep = '-')
  



























































































