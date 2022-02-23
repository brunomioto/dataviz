# packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggrepel)
library(extrafont)
library(readr)

# data --------------------------------------------------------------------

data <- read_csv("https://raw.githubusercontent.com/brunomioto/dataviz/main/pfizer_vaccine/pfizer_vaccine_trial.csv")

# plot --------------------------------------------------------------------

data %>% 
  ggplot(aes(x = days_after_dose_1))+
  #dif
  geom_segment(aes(x = 110, xend = 110, y = 0.29413854, yend = 2.24653641),
               color = "#bdbdbd",
               size = 2)+
  geom_label(aes(x = 110, y = 1.4, label = "7.6x"),
             color = "#bdbdbd",
             fill = "#fcfcfc",
             size = 8,
             label.padding = unit(10, "pt"),
             label.size = NA,
             hjust = 0.43)+
  #placebo
  geom_step(aes(y = placebo),
            color = "#f76833",
            size = 1)+
  geom_point(data = data %>%  
               distinct(placebo, .keep_all = TRUE),
             aes(y = placebo),
             color = "#f76833",
             shape = 21,
             fill = NA)+
  geom_text_repel(aes(label = ifelse(days_after_dose_1 == 73, "Placebo group",""),
                      y = placebo),
                  nudge_x = -5,
                  box.padding = 0.5,
                  nudge_y = 0.3,
                  segment.curvature = 0.1,
                  segment.ncp = 3,
                  segment.angle = 20,
                  fontface = "bold",
                  color = "#f76833",
                  size = 4)+
  #vaccine
  geom_step(aes(y = vaccine),
            color = "#196299",
            size = 1)+
  geom_point(data = data %>%  
               distinct(vaccine, .keep_all = TRUE),
             aes(y = vaccine),
             color = "#196299",
             shape = 21,
             fill = NA)+
  geom_text_repel(aes(label = ifelse(days_after_dose_1 == 96, paste("Vaccinated group\nBNT162b2 (30\u03bcg)"),""),
                      y = vaccine),
                  nudge_x = -5,
                  box.padding = 0.5,
                  nudge_y = 0.3,
                  segment.curvature = 0.1,
                  segment.ncp = 3,
                  segment.angle = 20,
                  fontface = "bold",
                  color = "#196299",
                  size = 4)+
  scale_y_continuous(breaks = seq(0,2.4,0.4), expand = expansion(mult = c(0,0.05)))+
  scale_x_continuous(breaks = seq(0,119,7), expand = expansion(mult = c(0.01,0.02)))+
  coord_cartesian(ylim = c(0,2.4))+
  labs(
    title = "COVID-19 incidence rates in Pfizer/BioNTech's vaccine trial",
    x = "Days after first dose",
    y = "Cumulative incidence (%)",
    caption = "Chart: Bruno Mioto @BrunoHMioto - Source: New England Journal of Medicine"
  )+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "#fcfcfc", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", size = 10),
    text = element_text(family = "Open Sans"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    plot.margin = margin(10,10,10,10, unit = "pt")
  )


#ggsave("pfizer_trial_english.png", width = 10, height = 7)
