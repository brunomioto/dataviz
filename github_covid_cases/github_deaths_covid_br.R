# packages ----------------------------------------------------------------

library(owidR)
library(dplyr)
library(ggplot2)
library(lubridate)
library(glue)

# data --------------------------------------------------------------------

owid_covid <- owidR::owid_covid()

owid_br <- owid_covid %>% 
  filter(location == "Brazil",
         date >= "2021-05-02") %>%
  # process versions of the date column
  mutate(date = lubridate::as_date(date),
         year = lubridate::year(date),
         week = lubridate::week(date),
         wday = lubridate::wday(date),
         week = ifelse(year == "2022", week+52, week))

deaths_last_year <- sum(owid_br$new_deaths)

# plot --------------------------------------------------------------------

owid_br %>%
  ggplot(aes(x = week, y = wday, fill = new_deaths)) +
  geom_tile(color = "#0d1116", 
            size = 1,
            width=0.85, height=0.85) +
  annotate("text", x = 66, y = 1, vjust = 2.9, hjust = 0, label = "More",
           color = "#c9d1d9")+
  annotate("text", x = 60, y = 1, vjust = 2.9, hjust = 1, label = "Less",
           color = "#c9d1d9")+
  scale_fill_gradientn(colours = rev(c("#38d353",
                                   "#28a541",
                                   "#006d31",
                                   "#0d4429",
                                   "#161b22")),
                                 guide = "bins",
                   breaks = seq(0,3000,600),
                   )+
  scale_x_continuous(expand = expansion(0.01),
                     position = "bottom",
                     sec.axis = sec_axis(
                       trans = ~scales::rescale(., to = c(0.5, 12.5)),
                       breaks = 1:12,
                       labels = lubridate::month(c(seq(5,12),seq(1,4)), T)
                       ))+
  labs(title = glue::glue("{deaths_last_year} deaths in the last year"))+
  coord_fixed(clip = "off") +
  theme_dark() +
  theme(
    panel.background = element_rect(fill = "#0d1116", color = NA),
    plot.background = element_rect(fill = "#0d1116", color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.x.top = element_text(color = "#c9d1d9", face = "bold", size = rel(1.2)),
    legend.direction = "horizontal",
    legend.position = c(0.85,-0.1),
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(15, "pt"),
    plot.title = element_text(color = "#c9d1d9", face = "bold", size = rel(1.2))
  )

ggsave("github_deaths_covid_br.png", width = 10, height = 2.4)
