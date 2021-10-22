library(nflfastR)
library(tidyverse)
library(glue)
library(ggdist)
library(ggthemes)
library(extrafont)
library(nflreadr)
library(nflplotR)
library(ggrepel)

# load data -------------------------------------------------------------

future::plan("multisession")
#pbp <- load_pbp(seasons = 2000:2021) #tom brady
#pbp <- load_pbp(seasons = 2004:2021) #big ben
pbp <- load_pbp(seasons = 2017:2021) #mahomes
#pbp <- load_pbp(seasons = 2005:2021) #rodgers

pbp_clean <- clean_pbp(pbp)

rosters <- load_rosters(2019:2021) #can be just 2021, but if the player hasn't played yet, load some year he played

# working in data -------------------------------------------------------------

QB_EPA <- pbp_clean %>%
  filter(!is.na(epa),
         passer_player_id == "00-0033873", #CHANGE IT FOR PLAYER SELECTION
         rush == 1 | pass == 1) %>%
  group_by(game_id) %>% 
  mutate(rows = n()) %>% 
  filter(rows >= 5) %>% 
  ungroup() %>% 
  group_by(game_id, name, passer_player_id, posteam, defteam, season, week) %>% 
  summarize(tot_epa = sum(epa),
            mean_epa = mean(epa)) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  left_join(rosters, by = c("passer_player_id" = "gsis_id",
                            "season" = "season"))

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

# plot -------------------------------------------------------------

ggplot(QB_EPA, aes(x=name, y=mean_epa))+
  #dist
  ggdist::stat_halfeye(aes(fill = last_game$team_color),
    adjust = .5, 
    width = .4, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA,
    alpha = 0.7
  ) + 
  #boxplot
  geom_boxplot(aes(color = last_game$team_color),
    width = .1, 
    outlier.shape = NA
  ) +
  #all games
  geom_point(aes(x=as.numeric(name)-0.2+jit,
                 color = last_game$team_color),
             shape = 19,
             size=4,
             alpha = 0.5)+
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
                 color = last_game$team_color2),
             shape = 19,
             size=4)+
  #last game
  geom_point(aes(x=as.numeric(name)-0.2+jit,
                 y=ifelse(game_id %in% last_game$game_id,
                          mean_epa,
                          NA),
                 color = last_game$team_color2,
                 fill = last_game$team_color),
             shape = 21,
             size=4,
             stroke = 1.5)+
  nflplotR::geom_nfl_headshots(data = last_game,
                               aes(player_gsis = last_game$passer_player_id,
                                   x=1.8,
                                   y=0.75),
                               width = 0.2,
                               stat = "unique")+
  scale_color_identity()+
  scale_fill_identity()+
  labs(
    title = glue::glue("EPA/play in each game of {last_game$full_name} career"),
    subtitle = glue::glue("Last game: {last_game$posteam} against {last_game$defteam} on week {last_game$week} of {last_game$season} season - EPA/play: {round(last_game$mean_epa, 2)}"),
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
    axis.text.x = element_text(face = "bold")
  )

#ggpreview(g, width = 5, asp = 16/9)
ggsave(filename = glue::glue("{last_game$name}_{last_game$game_id}.png"),
       width = 9,
       height = 5)

