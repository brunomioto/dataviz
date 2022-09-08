# packages ----------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(nflplotR)
library(nflfastR)
library(ggchicklet)
library(ggh4x)

# data --------------------------------------------------------------------
url <- "https://raw.githubusercontent.com/ajreinhard/NFL-public/main/misc-data/2013_to_2022_init53.csv"

rosters <- read_csv(url)

roster_2021 <- rosters %>% 
  filter(season == 2022) %>% 
  mutate(draft_rnd = ifelse(draft_type == "Undrafted","UFA",draft_rnd))

round_players <- roster_2021 %>% 
  count(team, draft_rnd) %>% 
  left_join(nflfastR::teams_colors_logos,
          by = c("team" = "team_abbr"))


round_players$team <- nfl_team_factor(round_players$team)

round_players$team_division <- factor(round_players$team_division,
                                      levels = c("AFC West","AFC North","AFC South","AFC East",
                                                 "NFC West","NFC North","NFC South","NFC East"))

round_players$draft_rnd <- factor(round_players$draft_rnd,
       levels = c("1","2","3","4","5","6","7","UFA"))
# plot --------------------------------------------------------------------

round_players %>% 
  ggplot(aes(x = team, y = n, fill = draft_rnd))+
  geom_chicklet()+
  geom_text(position = position_stack(vjust = 0.5,reverse = TRUE),
            aes(label = ifelse(n > 2, n, "")),
            color = "white")+
  facet_wrap2(~team_division, ncol = 4, scales = "free_y", axes = "all")+
  scale_y_continuous(breaks = seq(0,50,10),
                     expand = c(0,0))+
  scale_fill_manual(
    values = c('#dc3913',
               '#3366cc',
               '#0f9618',
               '#992299',
               '#0099c6',
               '#dd4477',
               '#e67300',
               '#737373')
  )+
  labs(
    title = "Number of players of each team by draft round in the 2022 season",
    fill = "Draft round",
    caption = "Bruno Mioto @BrunoHMioto - Data: TheFootballDB by @reinhurdler"
  )+
  guides(fill = guide_legend(nrow = 1,
                             label.position = "bottom"))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = rel(1.5), face = "bold"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15,15,15,15,unit = "pt"),
    legend.title = element_text(face = "bold"),
    axis.text.y = element_nfl_logo(size = 0.7),
    axis.title = element_blank(),
    legend.position = "top",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    legend.key.height =  unit(1, "cm"),
    legend.box.margin = margin(-5,0,-10,0,unit = "pt"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "#999999"),
    strip.text = element_text(size = rel(1.2),
                              face = "bold"),
    text = element_text(family = "Open Sans")
  )+
  coord_flip()+
  NULL

#ggsave("rnd_players_2022.png", width = 10, height = 6)










