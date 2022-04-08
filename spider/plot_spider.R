
# packages ----------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggthemes)
library(nflplotR)

# data --------------------------------------------------------------------

data <- read_csv("species_export_20220408.csv")


# plot --------------------------------------------------------------------

#point
plot <- data %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  ggplot(aes(x = year, y = cum_sum))+
  geom_line(color = "grey50")+
  geom_point(
    aes(color = ifelse(year == 2022, "#735133", "grey50"))
  )+
  annotate(
    geom = "curve", x = 1950, y = 45000, xend = 2022, yend = 50000, 
    curvature = -.2,
    color = "#735133"
  ) +
  geom_richtext(x = 1950, 
                y = 40000, 
                size = 4.5,
                color = "#735133",
                label.color = NA,
                fill = "#f0f0f0",
                label = "<i>Guriurius minuano</i><br>Marta, Bustamante, Ruiz &<br>Rodrigues, 2022"
  )+
  annotate(
    nflplotR::GeomFromPath,
    x = 1787, y = 40000,
    path = "./spider_picture.png",
    width = 0.2
  ) +
  #geom_from_path(path = "D:/OneDrive/Mioto_OneDrive/Bruno/Charts/spider/spider_picture.png",
  #               width = 0.15, x = 1800, y = 40000)+
  scale_color_identity()+
  scale_y_continuous(expand = expansion(mult = c(0.02,0.02)),
                     labels = scales::unit_format(big.mark = ",", unit = ""))+
  scale_x_continuous(breaks = c(seq(1750, 2022, 25),2022))+
  labs(
    title = "The world of spiders: The 50,000th species described!",
    x = "Year",
    y = "Number of species",
    caption = "Data: World Spider Catalog\nFigure:  Marta, Bustamante, Ruiz & Rodrigues (2022)"
  )+
  theme_fivethirtyeight() +
  theme(
    panel.grid.minor.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title.position = "plot",
    axis.title.x = element_text(margin = margin(5,0,-5,0)),
    plot.caption = element_text(margin = margin(0,0,0,0))
  )+
  coord_cartesian(ylim = c(0,NA))



ggsave(plot = plot, "spider.png", width = 8, height = 5)

###BR
#point
plot_br <- data %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  ggplot(aes(x = year, y = cum_sum))+
  geom_line(color = "grey50")+
  geom_point(
    aes(color = ifelse(year == 2022, "#735133", "grey50"))
  )+
  annotate(
    geom = "curve", x = 1950, y = 45000, xend = 2022, yend = 50000, 
    curvature = -.2,
    color = "#735133"
  ) +
  geom_richtext(x = 1950, 
                y = 40000, 
                size = 4.5,
                color = "#735133",
                label.color = NA,
                fill = "#f0f0f0",
                label = "<i>Guriurius minuano</i><br>Marta, Bustamante, Ruiz &<br>Rodrigues, 2022"
  )+
  annotate(
    nflplotR::GeomFromPath,
    x = 1787, y = 40000,
    path = "D:/OneDrive/Mioto_OneDrive/Bruno/Charts/spider/spider_picture.png",
    width = 0.2
  ) +
  scale_color_identity()+
  scale_y_continuous(expand = expansion(mult = c(0.02,0.02)),
                     labels = scales::unit_format(big.mark = ".", unit = ""))+
  scale_x_continuous(breaks = c(seq(1750, 2022, 25),2022))+
  labs(
    title = "O mundo das aranhas: A 50.000ª espécie descrita!",
    x = "Ano",
    y = "Número de espécies",
    caption = "Dados: World Spider Catalog\nFigura:  Marta, Bustamante, Ruiz & Rodrigues (2022)"
  )+
  theme_fivethirtyeight() +
  theme(
    panel.grid.minor.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title.position = "plot",
    axis.title.y = element_text(margin = margin(0,5,0,-5)),
    axis.title.x = element_text(margin = margin(5,0,-5,0)),
    plot.caption = element_text(margin = margin(0,0,0,0))
  )+
  coord_cartesian(ylim = c(0,NA))



ggsave(plot = plot_br, "spider_br.png", width = 8, height = 5)
