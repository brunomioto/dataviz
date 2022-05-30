
# packages ----------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(nflplotR)
library(ggforce)

# data --------------------------------------------------------------------

shoes <- read_csv("shoes_companies.csv")

shoes2 <- shoes %>% 
  mutate(logo = c("https://upload.wikimedia.org/wikipedia/commons/a/a6/Logo_NIKE.svg",
                  "https://upload.wikimedia.org/wikipedia/commons/e/ee/Logo_brand_Adidas.png",
                  "https://upload.wikimedia.org/wikipedia/commons/b/b1/Asics_Logo.svg",
                  "https://upload.wikimedia.org/wikipedia/en/4/49/Puma_AG.svg",
                  "https://upload.wikimedia.org/wikipedia/commons/4/44/Under_armour_logo.svg")) %>% 
  pivot_longer(
    cols = c("2016","2021"),
    names_to = "year",
    values_to = "sales"
  ) %>% 
  group_by(year) %>% 
  arrange(desc(sales)) %>% 
  mutate(ranking = row_number())

lines_left <- tibble(
  x = c(2015, 2017, 2017,2015),
  y = c(-2, 0.5, 6.5, 6)
)
lines_right <- tibble(
  x = c(2022, 2020, 2020,2022),
  y = c(-2, 0.5, 6.5, 6)
)

# plot --------------------------------------------------------------------

shoes2 %>% 
  ggplot(aes(x = as.integer(year), y = ranking, group = company, color = company))+
  stat_bezier(geom = "polygon", data = lines_left, aes(x = x, y = y), group = 1,
              color = "#515151",fill = "#fafafa")+
  stat_bezier(geom = "polygon", data = lines_right, aes(x = x, y = y), group = 1,
              color = "#515151",fill = "#fafafa")+
  geom_point(size = 8, color = "white")+
  geom_line(size = 6, color = "white")+
  geom_point(size = 5, color = "black")+
  geom_line(size = 3, lineend = "round")+
  geom_from_path(data = filter(shoes2, year == 2021), aes(path = logo), width = 0.1, hjust = -0.3)+
  annotate("text", x = 2015.8, y = 0.6, label = "2016", fontface = "bold", family = "Open Sans",
           size = 6)+
  annotate("text", x = 2021.2, y = 0.6, label = "2021", fontface = "bold", family = "Open Sans",
           size = 6)+
  geom_text(data = filter(shoes2, year == 2016), aes(label = paste0("#", ranking)),
            hjust = 1.6, fontface = "bold", family = "Open Sans", size = 5)+
  scale_y_continuous()+
  scale_x_continuous(expand = c(0,0))+
  scale_color_manual(
    values = c(
      "Nike" = "#ff6600",
      "Adidas" = "#1d6cb3",
      "Puma" = "#f60000",
      "Asics" = "#002261",
      "Under Armor" = "#000000"
    )
  )+
  labs(
    title = "Tighten your shoelaces",
    subtitle = "Worldwide footwear sales ranking of selected sports brands",
    caption = "Bruno Mioto @BrunoHMioto\nData: Company Reports/Statista"
  )+
  theme_classic()+
  theme(
    plot.background = element_rect(fill = "#ebebeb", color = NA),
    panel.background = element_rect(fill = "#ebebeb", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = rel(1.6)),
    plot.subtitle = element_text(hjust = 0.5, size = rel(0.9)),
    panel.grid = element_blank(),
    panel.grid = element_line(),
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(family = "Open Sans")
  )+
  coord_cartesian(xlim = c(2015.2,2021.8), ylim = c(5.8,0.5), clip = "off")


ggsave("shoes.png", width = 6, height = 7)
