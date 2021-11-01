library(nflfastR) #nfl data
library(nflreadr) #nfl data
library(nflplotR) #player headshot
library(tidyverse) #everything
library(glue) #dynamic text
library(ggdist) #distribution plot
library(ggthemes) #ggplot themes
library(extrafont) #custom fonts
library(ggrepel) #repel overlapping text
library(ggtext) #new text tools

pbp <- load_pbp(seasons = 2009:2021) #stafford games

rosters <- load_rosters(2021)

QB_EPA <- pbp %>%
  filter(!is.na(epa),
         passer_id ==  "00-0026498",
         rush == 1 | pass == 1,
         down <= 4
         ) %>%
  group_by(game_id) %>% 
  mutate(rows = n()) %>% 
  filter(rows >= 5) %>% 
  ungroup() %>% 
  group_by(game_id, name, passer_id, posteam, defteam, season, week) %>% 
  summarize(tot_epa = sum(epa),
            mean_epa = mean(epa)) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  left_join(rosters, by = c("passer_id" = "gsis_id",
                            "season" = "season"))

#new rams colors - just for rams games
QB_EPA$team_color[QB_EPA$team_color == "#002244"] <- "#003594"
QB_EPA$team_color2[QB_EPA$team_color2 == "#b3995d"] <- "#ffd100" 

QB_EPA$name <- as.factor(QB_EPA$name)

last_game <- QB_EPA %>% 
  filter(season == max(QB_EPA$season)) %>%
  arrange(week) %>% 
  tail(1)

this_season <- QB_EPA %>% 
  filter(season == 2021) %>% 
  mutate(dif = abs(mean_epa-last(QB_EPA$mean_epa))) #find which game has the greater difference in epa/play, just for labelling it better
  
dif_game <- this_season %>%
  dplyr::filter(dif %in% max(this_season$dif))

set.seed(2000)
QB_EPA$jit <- runif(nrow(QB_EPA), -0.1, 0.1) #create jitter effect because it's terrible working with it.

percentile_epa_value <- quantile(QB_EPA$mean_epa, stats::ecdf(QB_EPA$mean_epa)(last_game$mean_epa), type = 1)

percentile_epa_percent <- round(stats::ecdf(QB_EPA$mean_epa)(last_game$mean_epa)*100)

# single_plot -------------------------------------------------------------

ggplot(QB_EPA, aes(x=name, y=mean_epa))+
  #dist
  ggdist::stat_halfeye(aes(fill = stat(y < percentile_epa_value)),
    adjust = .5, 
    width = .4, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA,
    alpha = 0.7
  ) +
  #boxplot
  geom_boxplot(aes(color = last_game$team_color),
    width = .1, 
    outlier.shape = NA
  ) +
  #triangle
  geom_point(aes(x=as.numeric(name)+0.11,
                 y=ifelse(game_id %in% last_game$game_id,
                          mean_epa,
                          NA),
                 color = last_game$team_color2,
                 fill = "last_game$team_color"),
             shape = 24,
             size=4,
             stroke = 1.1,
             stat = "unique")+
  #all games
  geom_point(aes(x=as.numeric(name)-0.2+jit,
                 color = team_color),
             shape = 19,
             size=4,
             alpha = 0.4)+
  geom_text_repel(aes(x = as.numeric(name)-0.2+jit,
                      label = ifelse(game_id %in% last_game$game_id, "Last game",
                                     ifelse(game_id %in% dif_game$game_id,"2021 season games", ""))),
                  nudge_x = -0.2-QB_EPA$jit,
  )+
  #this season
  geom_point(aes(x=as.numeric(name)-0.2+jit,
                 y=ifelse(game_id %in% this_season$game_id,
                          mean_epa,
                          NA),
                 fill = "last_game$team_color2"),
             color = "black",
             shape = 21,
             size=4)+
  #last game
  geom_point(aes(x=as.numeric(name)-0.2+jit,
                 y=ifelse(game_id %in% last_game$game_id,
                          mean_epa,
                          NA),
                 color = last_game$team_color2,
                 fill = "last_game$team_color"),
             shape = 21,
             size=4,
             stroke = 1.2)+
  #player headshot
  nflplotR::geom_nfl_headshots(data = last_game,
                               aes(player_gsis = last_game$passer_id,
                                   x=1.8,
                                   y=0.75),
                               width = 0.2,
                               stat = "unique")+
  geom_richtext(aes(x = 1.7,
                y = -0.7,
                label = glue::glue("**Last game**<br>{last_game$posteam} vs {last_game$defteam} on week {last_game$week} of {last_game$season} season<br>EPA/play: {round(last_game$mean_epa, 2)}<br>Better than {percentile_epa_percent}% of his career games"),
                ),
                stat = "unique",
                fill = "white",
                label.color = last_game$team_color
  )+
  scale_color_identity()+
  #density plot colors messed the colors, this is a workaround. I hated it
  scale_fill_manual(values = c("FALSE" = "gray48",
                               "TRUE" = last_game$team_color,
                               "last_game$team_color" = last_game$team_color,
                               "last_game$team_color2" = last_game$team_color2))+
  labs(
    title = glue::glue("EPA/play in each game of {last_game$full_name} career"),
    subtitle = "",
    y="Mean EPA/play",
    x="",
    caption = "Bruno Mioto @BrunoHMioto - Data: nflfastR"
  )+
  coord_flip(ylim = c(-1, 1),
             clip = "off")+
  theme_fivethirtyeight()+
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_blank(),
    text = element_text(family = "Open Sans"),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "none",
  )

#ggpreview(g, width = 5, asp = 16/9)
ggsave(filename = glue::glue("{last_game$name}_{last_game$game_id}.png"),
       width = 9,
       height = 5)
