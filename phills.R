rm(list = ls())

library(baseballr)
library(tidyr)
library(dplyr)
library(ggplot2)

# Import data on Nola and Wheeler
nola <- playerid_lookup(last_name = "Nola",
                        first_name = "Aaron") %>% 
  select(mlbam_id, first_name, last_name)
wheeler <- playerid_lookup(last_name = "Wheeler",
                           first_name = "Zack") %>% 
  select(mlbam_id, first_name, last_name) 

suarez <- playerid_lookup(last_name = "Suarez",
                          first_name = "Ranger") %>% 
  select(mlbam_id, first_name, last_name)

nola_post <- statcast_search("2022-10-07", "2022-10-28", 
                             playerid = nola[1, 1], 
                             player_type = "pitcher")
wheeler_post <- statcast_search("2022-10-07", "2022-10-28", 
                                playerid = wheeler[1, 1],
                                player_type = "pitcher")
suarez_post <- statcast_search("2022-10-07", "2022-10-28",
                               playerid = suarez[1, 1],
                               player_type = "pitcher")

unique(nola_post$game_type)     # W L D F
unique(wheeler_post$game_type)  #   L D F

game_type = c("W", "L", "D", "F")

clean_nola <- nola_post %>% 
  dplyr::filter(!is.na(pfx_x), !is.na(pfx_z),
                nola_post$game_type %in% game_type) %>% 
  dplyr::mutate(pfx_in_px = -12 * pfx_x,
                pfz_in_pz =  12 * pfx_z)

clean_wheeler <- wheeler_post %>% 
  dplyr::filter(!is.na(pfx_x), !is.na(pfx_z),
                wheeler_post$game_type %in% game_type) %>% 
  dplyr::mutate(pfx_in_px = -12 * pfx_x,
                pfz_in_pz =  12 * pfx_z)

clean_suarez <- suarez_post %>% 
  dplyr::filter(!is.na(pfx_x), !is.na(pfx_z),
                suarez_post$game_type %in% game_type) %>% 
  dplyr::mutate(pfx_in_px = -12 * pfx_x,
                pfz_in_pz =  12 * pfx_z)

# set coloring for pitches
pitch_colors <- c("CH" = "red",          # Changeup
                  "FF" = "blue",         # Four-seam Fastball
                  "KC" = "green",        # Knuckle-curve
                  "FC" = "cyan",         # Cutter
                  "SI" = "yellow",       # Sinker
                  "SL" = "pink",         # Slider
                  "CU" = "black")        # Curveball

game_color <- c("W" = "red",
                "L" = "green",
                "D" = "blue",
                "F" = "black")

nola_pt <- unique(clean_nola$pitch_type)
nola_gm <- unique(clean_nola$game_type)
nola_pitches <- c("Changeup", "Four-seam Fastball", "Knuckle-curve", 
                  "Cutter", "Sinker")
nola_games <- c("World Series", "League Championship Series",
                "Divisional Series", "Wild Card")

wheeler_pt <- unique(clean_wheeler$pitch_type)
wheeler_gm <- unique(clean_wheeler$game_type)
wheeler_pitches <- c("Slider", "Four-seam Fastball", "Sinker", 
                     "Curveball", "Changeup")
wheeler_games <- c("League Championship Series",
                "Divisional Series", "Wild Card")

suarez_pt <- unique(clean_suarez$pitch_type)
suarez_gm <- unique(clean_suarez$game_type)
suarez_pitches <- c("Four-seam Fastball", "Cutter", "Sinker", "Curveball",
                    "Changeup", "Slider")
suarez_games <- c("World Series", "League Championship Series",
                "Divisional Series")

plot_pitchers <- function(df, pt, name, lbs) {
  header = paste(name, "Pitch Movement")
  df %>% 
    ggplot(aes(x = pfx_in_px, y = pfz_in_pz, color = pitch_type)) + 
    geom_vline(xintercept = 0) + 
    geom_hline(yintercept = 0) + 
    geom_point(size = 1.5, alpha = 0.45) + 
    scale_color_manual(values = pitch_colors,
                       limits = pt,
                       labels = lbs) + 
    scale_x_continuous(limits = c(-25, 25),
                       breaks = c(-20, 20, 5),
                       labels = scales::number_format(suffix = "\"")) + 
    scale_y_continuous(limits = c(-25, 25),
                       breaks = c(-20, 20, 5),
                       labels = scales::number_format(suffix = "\"")) +
    coord_equal() + 
    labs(title = header,
         subtitle = "2022 Post Season | Pitcher's POV",
         caption = "Data: Baseball Savant via baseballr",
         x = "Horizontal Break",
         y = "Induced Vertical Break",
         color = "Pitch Name")
}

plot_pitchers(clean_nola, nola_pt, "Aaron Nola", nola_pitches)
plot_pitchers(clean_wheeler, wheeler_pt, "Zack Wheeler", wheeler_pitches)
plot_pitchers(clean_suarez, suarez_pt, "Ranger Suarez", suarez_pitches)

plot_by_series <- function(df, pt, name, lbs) {
  header = paste(name, "Pitches by Series")
  df %>% 
    ggplot(aes(x = pfx_in_px, y = pfz_in_pz, color = game_type)) + 
    geom_vline(xintercept = 0) + 
    geom_hline(yintercept = 0) + 
    geom_point(size = 1.5, alpha = 0.45) + 
    scale_color_manual(values = game_color,
                       limits = pt,
                       labels = lbs) + 
    scale_x_continuous(limits = c(-25, 25),
                       breaks = c(-20, 20, 5),
                       labels = scales::number_format(suffix = "\"")) + 
    scale_y_continuous(limits = c(-25, 25),
                       breaks = c(-20, 20, 5),
                       labels = scales::number_format(suffix = "\"")) +
    coord_equal() + 
    labs(title = header,
         subtitle = "2022 Post Season | Pitcher's POV",
         caption = "Data: Baseball Savant via baseballr",
         x = "Horizontal Break",
         y = "Induced Vertical Break",
         color = "Pitch Name")
}

plot_by_series(clean_nola, nola_gm, "Aaron Nola", nola_games)
plot_by_series(clean_wheeler, wheeler_gm, "Zack Wheeler", wheeler_games)
plot_by_series(clean_suarez, suarez_gm, "Ranger Suarez", suarez_games)