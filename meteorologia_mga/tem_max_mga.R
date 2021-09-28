library(basedosdados)
library(tidyverse)
library(lubridate)
library(extrafont)
library(cowplot)
library(RColorBrewer)

# base dos dados ----------------------------------------------------------

estacao_mga <- bdplyr("basedosdados.br_inmet_bdmep.estacao") %>%
  filter(id_municipio == "4115200") %>% 
  bd_collect()

dados_estacao_mga <- bdplyr("basedosdados.br_inmet_bdmep.microdados") %>%
  filter(id_estacao == "A835") %>% 
  select(ano, data, hora, temperatura_min, temperatura_max) %>% 
  bd_collect()

#tabela_falta <- dados_estacao_mga %>% 
#  filter(is.na(temperatura_max)) %>% 
#  group_by(data) %>% 
#  summarise(dia = day(data), temp_max = max(temperatura_max)) %>% 
#  unique()

# data wrangling ----------------------------------------------------------

temp_mga <- dados_estacao_mga %>% 
  mutate(yday = yday(data),
         year = year(data)) %>% 
  filter(year != 2006,
         year != 2009,
         year != 2010) %>% 
  group_by(data) %>% 
  summarise(data,
            yday = yday,
            year = year,
            temp_max = max(temperatura_max),
            temp_min = min(temperatura_min)) %>% 
  unique()


meses <- tibble(yday = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                 label = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))


myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
myPalette(13)

# plot --------------------------------------------------------------------

ggplot(temp_mga, aes(x = yday,
                     y = temp_max,
                     color = as.factor(year)), )+
  geom_point(
    shape = 16,
    alpha = 0.5,
    show.legend = FALSE
    )+
  geom_smooth(
    size = 1.2,
    key_glyph = rectangle_key_glyph(fill = color, alpha = 1),
    method = 'loess',
    formula = 'y ~ x',
    se = FALSE
  )+
  scale_color_manual(
    breaks = c(
      2007,
      2008,
      2011,
      2012,
      2013,
      2014,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2021
    ),
    values = c(
      "#5E4FA2",
      "#397EB8",
      "#54AEAD",
      "#88CFA4",
      "#BEE4A0",
      "#EAF69E",
      "#FFFFBF",
      "#FEE593",
      "#FDBE6F",
      "#F88D52",
      "#E95D46",
      "#CB334C",
      "#000000"
    ),
    labels = c(
      "  2007  ",
      "  2008  ",
      "  2011  ",
      "  2012  ",
      "  2013  ",
      "  2014  ",
      "  2015  ",
      "  2016  ",
      "  2017  ",
      "  2018  ",
      "  2019  ",
      "  2020  ",
      "  2021  "
    )
  )+
  scale_x_continuous(
    breaks = meses$yday,
    labels = meses$label,
    expand = c(.001, .001)
  )+
  scale_y_continuous(
    breaks = seq(10, 40, by = 5),
    labels = function(x) paste0(x, " °C")
  )+
  guides(
    color = guide_legend(
      label.position = "bottom",
      nrow = 1,
      byrow = TRUE
      )
    )+
  theme_minimal()+
  theme(
    text = element_text(family = "Open Sans"),
    title = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "grey20"),
    axis.ticks.x = element_line(color = "grey20"),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey20", size = 14),
    legend.position = c(0.4, 0.935),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.text = element_text(color = "grey20", size = 12),
    plot.margin = margin(10, 20, 10, 20),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(color = "grey20", size = 10),
    )+
  labs(title = "Temperatura diária máxima em Maringá-PR (2007-2021)",
       subtitle = "Foram retirados anos com dados insuficientes",
       caption = "Dados: INMET obtidos com a basedosdados.org • Gráfico: Bruno Mioto @BrunoHMioto") +
  coord_cartesian(ylim = c(12.5, 40))

ggsave("temp_max_mga.png", width = 12, height = 8)
