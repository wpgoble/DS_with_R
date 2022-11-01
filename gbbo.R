library(bakeoff)
library(tidyverse)

ratings <- bakeoff::ratings %>% 
  mutate(series = as.factor(series))

seasons <- bakeoff::seasons_raw

glimpse(ratings)

# Create continuous episode count
plot_off1 <- ratings %>% 
  mutate(ep_id = row_number()) %>% 
  select(ep_id, viewers_7day, series, episode)

# Create coordinates for labels
series_label <- plot_off1 %>% 
  group_by(series) %>% 
  summarize(x_position = mean(ep_id),
            y_position = median(viewers_7day) + 1)

# Make the plot
ggplot(plot_off1, aes(x = ep_id, y = viewers_7day, fill = series)) +
  geom_col(alpha = 0.9) +
  ggtitle("Series 8 was a Big Setback in Viewers",
          subtitle = "7-Day Viewers Across All Series/Episodes") + 
  geom_text(data = series_label, aes(label = series,
                                     x = x_position,
                                     y = y_position)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_fill_bakeoff(guide = "none") + 
  scale_x_continuous(expand = c(0, 0))

plot_off5 <- ratings %>% 
  select(series, episode, viewers_7day) %>% 
  group_by(series) %>% 
  filter(episode == 1 | episode == max(episode)) %>% 
  mutate(episode = recode(episode, '1' = "first", .default = "last")) %>% 
  ungroup()

ggplot(plot_off5, aes(x = viewers_7day,
                      y = fct_rev(series),
                      color = episode,
                      group = series)) + 
  geom_line(size = 0.75) + 
  geom_point(size = 2.5) + 
  scale_color_bakeoff() + 
  labs(y = "Series", x = "Viewers (in millions)", color = "Episode") + 
  ggtitle("Great British Bake Off Finales Get More Viewers than Premieres")

# wrangling to calculate percent change
plot_off9 <- ratings %>% 
  select(series, episode, viewers_7day) %>% 
  group_by(series) %>% 
  filter(episode == 1 | episode == max(episode)) %>% 
  ungroup() %>% 
  mutate(episode = recode(episode, '1' = "first", .default = "last")) %>% 
  spread(episode, viewers_7day) %>% 
  mutate(pct_change = (last - first) / first)

ggplot(plot_off9, aes(x = fct_rev(series),
                      y = pct_change)) +
  geom_point(color = bakeoff_colors("bluesapphire"), size = 2) +
  geom_segment(aes(xend = fct_rev(series), yend = 0), color = bakeoff_colors("bluesapphire")) +
  geom_text(aes(label = scales::percent(pct_change)), hjust = -.25) +
  labs(x = "Series", y = "% Change in Viewers from First to Last Episode") +
  ggtitle("Percent Increase in Viewers was the Smallest for Series 8",
          subtitle= "Finale 7-day Viewers Relative to Premiere") +
  scale_y_continuous(labels = scales::percent, limits = c(0, .85)) +
  coord_flip()
