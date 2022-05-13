# packages ----------------------------------------------------------------

library(sidrar)
library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)
library(ggrepel)

# data --------------------------------------------------------------------

data_desemprego <- get_sidra(x = 4099, period = c(last = "360"))

data_ipca <- get_sidra(x = 1737, period = c(last = "360"))

# data wrangling ----------------------------------------------------------

data_desemprego2 <- data_desemprego %>%
  clean_names() %>%
  filter(variavel_codigo == 4099) %>%
  select(valor, trimestre_codigo) %>%
  add_row(valor = 11.1, trimestre_codigo = "202201") %>%
  rename(valor_emprego = valor)

data_ipca2 <- data_ipca %>%
  clean_names() %>%
  separate(col = mes,into = c("mes", "ano"), sep = " ") %>%
  filter(mes %in% c("março", "junho", "setembro", "dezembro"),
         variavel_codigo == 2265,
         ano >= 2012) %>%
  mutate(trimestre_codigo = case_when(mes == "março" ~ paste0(ano, "01"),
                                     mes == "junho" ~ paste0(ano, "02"),
                                     mes == "setembro" ~ paste0(ano, "03"),
                                     mes == "dezembro" ~ paste0(ano, "04")),
         tri = case_when(mes == "março" ~ 01,
                         mes == "junho" ~ 02,
                         mes == "setembro" ~ 03,
                         mes == "dezembro" ~ 04),
         presidente = case_when(ano >= 2012 & ano < 2015 ~ "DILMA 1",
                                ano >= 2015 & ano < 2016 ~ "DILMA 2",
                                ano == 2016 & tri == 01 ~ "GOVERNO INTERINO",
                                (ano == 2016 & tri > 01) | (ano > 2016 & ano < 2019) ~ "TEMER",
                                ano >= 2019 ~ "BOLSONARO"
                                )) %>%
  select(valor, trimestre_codigo, presidente, tri, ano) %>%
  rename(valor_ipca = valor)

data_combinado <- data_desemprego2 %>%
  left_join(data_ipca2)

# plot --------------------------------------------------------------------

data_combinado %>%
  mutate(trimestre_codigo = as.numeric(trimestre_codigo)) %>%
  arrange(trimestre_codigo) %>%
  ggplot(aes(x = valor_emprego, y = valor_ipca, color = lag_b))+
  geom_path(size = 1.5)+
  annotate("curve", x = 8.4, xend = 7.5, y = 8.89, yend = 9,
           arrow = arrow(length = unit(5, "pt")),curvature = 0.3,
           color = "grey40")+
  annotate("text", x = 7, y = 8.9, label = "Dados\ntrimestrais",
           family = "Nunito", lineheight = 0.8,
           color = "grey40")+
  geom_point(aes(shape = ifelse(tri == 1, NA, 21)),
                 size = 2, fill = "white")+
  geom_point(aes(shape = ifelse(tri != 1, NA, 16)),
             size = 2, color = "#010101")+
  geom_text_repel(aes(label = ifelse(tri == 1,
                                     paste0(tri, "º tri\n", ano),
                                     "")),
                  force_pull = 0,
                  box.padding = 0,
                  force = 0,
                  min.segment.length = 100,
                  #por algum motivo a ordem é zoada
                  position = position_nudge_repel(x = c(0.4, #2012
                                                        0.4, #2015
                                                        0.4, #2018
                                                        -0.45, #2021
                                                        0.4, #2013
                                                        0.4, #2016
                                                        -0.2, #2019
                                                        0.4, #2022
                                                        0.4, #2014
                                                        -0.45, #2017
                                                        0.2 #2020
                                                        ),
                                                  y = c(0, #2012
                                                        -0.1, #2015
                                                        0.1, #2018
                                                        0, #2021
                                                        0, #2013
                                                        0.1, #2016
                                                        0.3, #2019
                                                        0, #2022
                                                        -0.35, #2014
                                                        -0.1, #2017
                                                        0.3 #2020
                                                        )),
                  color = "black",
                  family = "Nunito",
                  fontface = "bold",
                  lineheight = 0.8,
                  seed = 13)+
  annotate("text", x = 7.5, y = 4.6, label = "DILMA 1", color = "#ff0000", fontface = "bold",
           size = 4.5, family = "Nunito")+
  annotate("text", x = 8.2, y = 10, label = "DILMA 2", color = "#01cacd", fontface = "bold",
           size = 4.5, family = "Nunito")+
  annotate("text", x = 10.4, y = 9, label = "GOVERNO\nINTERINO", color = "#e5b12e", fontface = "bold",
           size = 4.5, family = "Nunito", lineheight = 0.8)+
  annotate("text", x = 11.5, y = 6.5, label = "TEMER", color = "#ff8e4d", fontface = "bold",
           size = 4.5, family = "Nunito")+
  annotate("text", x = 13.5, y = 10.5, label = "BOLSONARO", color = "#cf69d1", fontface = "bold",
           size = 4.5, family = "Nunito")+
  scale_x_continuous(breaks = seq(6,15,1), expand = c(0,0))+
  scale_y_continuous(breaks = seq(2,12,1), expand = c(0,0))+
  scale_shape_identity()+
  scale_color_manual(values = c("DILMA 1" = "#ff0000",
                                "DILMA 2" = "#01cacd",
                                "GOVERNO INTERINO" = "#ffcf31",
                                "TEMER" = "#ff8e4d",
                                "BOLSONARO" = "#cf69d1"))+
  labs(x = "Desemprego (%)",
       y = "Inflação (%)",
       title = "A inflação e o desemprego no Brasil (2012-2022)",
       caption = "Gráfico: Bruno Mioto @BrunoHMioto com base no Nexo Jornal\nDados: IBGE")+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none",
    text = element_text(family = "Nunito"),
    axis.title = element_text(size = 12)
  )+
  coord_fixed(ylim = c(2,11.5), xlim = c(6,15))


ggsave("plot.png", width = 6.5, height = 7)
