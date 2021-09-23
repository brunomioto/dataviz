library("basedosdados")
library("tidyverse")
library("stringr")
library("reshape2")
library("extrafont")

# base dos dados ----------------------------------------------------------

tabela_mga <- bdplyr("basedosdados.br_ms_vacinacao_covid19.microdados") %>%
  filter(sigla_uf == "PR") %>% 
  filter(id_municipio_estabelecimento == "4115200") %>% 
  select(data_aplicacao_vacina,
         idade_paciente,
         sexo_paciente,
         dose_vacina) %>%  
  bd_collect()

# data wrangling ----------------------------------------------------------

tabela_mga$sexo_paciente <- as.factor(tabela_mga$sexo_paciente)
tabela_mga$dose_vacina <- as.factor(tabela_mga$dose_vacina)

tabela_mga2 <- tabela_mga %>% 
  mutate(dose_vacina = ifelse(str_detect(dose_vacina, "1"), 1, 2)) %>% 
  filter(sexo_paciente != "I")

piramide_vacinados <- tabela_mga2 %>% 
  mutate(grupo_idade = case_when(
    idade_paciente <=4 ~ "0_4",
    idade_paciente >=5 & idade_paciente <=9 ~ "5_9",
    idade_paciente >=10 & idade_paciente <=14 ~ "10_14",
    idade_paciente >=15 & idade_paciente <=19 ~ "15_19",
    idade_paciente >=20 & idade_paciente <=24 ~ "20_24",
    idade_paciente >=25 & idade_paciente <=29 ~ "25_29",
    idade_paciente >=30 & idade_paciente <=34 ~ "30_34",
    idade_paciente >=35 & idade_paciente <=39 ~ "35_39",
    idade_paciente >=40 & idade_paciente <=44 ~ "40_44",
    idade_paciente >=45 & idade_paciente <=49 ~ "45_49",
    idade_paciente >=50 & idade_paciente <=54 ~ "50_54",
    idade_paciente >=55 & idade_paciente <=59 ~ "55_59",
    idade_paciente >=60 & idade_paciente <=64 ~ "60_64",
    idade_paciente >=65 & idade_paciente <=69 ~ "65_69",
    idade_paciente >=70 & idade_paciente <=74 ~ "70_74",
    idade_paciente >=75 & idade_paciente <=79 ~ "75_79",
    idade_paciente >=80 ~ "80+"
  )) %>% 
  select(grupo_idade, sexo_paciente, dose_vacina) %>% 
  group_by(grupo_idade, sexo_paciente, dose_vacina) %>% 
  summarise(n = dplyr::n())

piramide_vacinados$n <- as.numeric(piramide_vacinados$n)

piramide_vacinados_tibble <- as_tibble(piramide_vacinados)

piramide_vacinados_tibble <- piramide_vacinados_tibble %>% 
  mutate(n_piramide = ifelse(sexo_paciente == "M", n*(-1), n))

piramide_vacinados_tibble$grupo_idade <- factor(piramide_vacinados_tibble$grupo_idade,
                                         levels = c( "0_4",
                                                     "5_9",
                                                     "10_14",
                                                     "15_19",
                                                     "20_24",
                                                     "25_29",
                                                     "30_34",
                                                     "35_39",
                                                     "40_44",
                                                     "45_49",
                                                     "50_54",
                                                     "55_59",
                                                     "60_64",
                                                     "65_69",
                                                     "70_74",
                                                     "75_79",
                                                     "80+"))

piramide_vacinados_final <- piramide_vacinados_tibble %>% 
  rename(faixa_etaria = grupo_idade,
         sexo = sexo_paciente)

# pop projetada maringá-pr ------------------------------------------------

pop_mga_sexo <- read.csv("https://raw.githubusercontent.com/brunomioto/pop_proj_mga/main/pop_proj_mga.csv")

pop_mga_sexo_tidy <- melt(pop_mga_sexo,
                          value.name = "pop",
                          variable.name = "sexo",
                          id.vars = "faixa_etaria")


# join pop_vacinados ------------------------------------------------------

piramide_vacinados_pop <- pop_mga_sexo_tidy %>% 
  left_join(piramide_vacinados_final)

piramide_vacinados_pop$faixa_etaria <- factor(piramide_vacinados_pop$faixa_etaria,
                                              levels = c( "0_4",
                                                          "5_9",
                                                          "10_14",
                                                          "15_19",
                                                          "20_24",
                                                          "25_29",
                                                          "30_34",
                                                          "35_39",
                                                          "40_44",
                                                          "45_49",
                                                          "50_54",
                                                          "55_59",
                                                          "60_64",
                                                          "65_69",
                                                          "70_74",
                                                          "75_79",
                                                          "80+"))

level_order <- c( "0_4",
                  "5_9",
                  "10_14",
                  "15_19",
                  "20_24",
                  "25_29",
                  "30_34",
                  "35_39",
                  "40_44",
                  "45_49",
                  "50_54",
                  "55_59",
                  "60_64",
                  "65_69",
                  "70_74",
                  "75_79",
                  "80+")
piramide_vacinados_pop <- piramide_vacinados_pop %>% 
  mutate(pop_piramide = ifelse(sexo == "M", pop*(-1),pop))

piramide_vacinados_pop <- piramide_vacinados_pop %>% 
  mutate(porc = ifelse(sexo == "M",
                       round((n_piramide*(-1)/pop)*100, digits = 1),
                       round((n_piramide/pop)*100, digits = 1)))
cols <- c(
  "População estimada (Masculino)" = "#ffc3a4",
  "1ª Dose (M)" = "#EE5A45",
  "2ª Dose ou Única (M)" = "#790000",
  "População estimada (Feminino)" = "#7bd1cb",
  "1ª Dose (F)" = "#1E8F89",
  "2ª Dose ou Única (F)" = "#00322f")

# plot --------------------------------------------------------------------

ggplot(piramide_vacinados_pop, aes(x = factor(faixa_etaria, level = level_order) , y = n_piramide)) +
  #populacao
  geom_bar(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("F"),] %>% 
             distinct(pop, .keep_all = TRUE),
           aes(x = faixa_etaria,
               y = pop_piramide,
               fill = "População estimada (Feminino)"),
           stat = "identity") + 
  geom_bar(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("M"),] %>% 
             distinct(pop, .keep_all = TRUE),
           aes(x = faixa_etaria,
               y = pop_piramide,
               fill = "População estimada (Masculino)"),
           stat = "identity") +
  #1_dose
  geom_bar(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("F"),] %>% 
             filter(dose_vacina == '1'),
           aes(fill = "1ª Dose (F)"),
           stat = "identity") + 
  geom_bar(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("M"),] %>% 
             filter(dose_vacina == '1'),
           aes(fill = "1ª Dose (M)"),
           stat = "identity") + 
  #2_dose
  geom_bar(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("F"),] %>% 
             filter(dose_vacina == '2'),
           aes(fill = "2ª Dose ou Única (F)"),
           stat = "identity") + 
  geom_bar(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("M"),] %>% 
             filter(dose_vacina == '2'),
           aes(fill = "2ª Dose ou Única (M)"),
           stat = "identity") + 
  #texto_1_dose
  geom_text(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("F"),] %>% 
              filter(dose_vacina == '1'),
            aes(x=faixa_etaria, y=ifelse(porc < 1, 50000, n_piramide),
                label = ifelse(faixa_etaria == "80+",
                               paste0(porc, "% do estimado (1ª Dose)"),
                               paste0(porc, "%")),
                hjust = 0),
            nudge_y = 100,
            
            fontface = "bold"
  )+ 
  geom_text(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("M"),] %>% 
              filter(dose_vacina == '1'),
            aes(x=faixa_etaria, y=ifelse(porc < 1, 50000, n_piramide),
                label = ifelse(faixa_etaria == "80+",
                               paste0(porc, "% do estimado (1ª Dose)"),
                               paste0(porc, "%")),
                hjust = 1),
            nudge_y = -100,
            fontface = "bold"
  )+ 
  #texto_2_dose
  geom_text(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("F"),] %>% 
              filter(dose_vacina == '2'),
            aes(x=faixa_etaria, y=ifelse(porc < 5, 50000, n_piramide),
                label = ifelse(faixa_etaria == "80+",
                               paste0(porc, "% (2ª Dose)"),
                               paste0(porc, "%")),
                hjust = ifelse(porc<50, 0, 1),
                fontface = "bold"),
            nudge_y = ifelse(piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("F"),] %>% 
                               filter(dose_vacina == '2') %>% 
                               select(porc)<50, 100, -100),
            
            color = "white"
  )+ 
  geom_text(data=piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("M"),] %>% 
              filter(dose_vacina == '2'),
            aes(x=faixa_etaria, y=ifelse(porc < 5, 50000, n_piramide),
                label = ifelse(faixa_etaria == "80+",
                               paste0(porc, "% (2ª Dose)"),
                               paste0(porc, "%")),
                hjust = ifelse(porc<50, 1, 0),
                fontface = "bold"),
            nudge_y = ifelse(piramide_vacinados_pop[piramide_vacinados_pop$sexo %in% c("M"),] %>% 
                               filter(dose_vacina == '2') %>% 
                               select(porc)<50, -100, 100),
            color = "white"
  )+ 
  #texto estimativa
  geom_text(aes(x="10_14", y=-20000),
            label = "População estimada pelo IPARDES (2021)",
            fontface = "bold",
            hjust = 0
  )+
  geom_curve(aes(x = 3.2, y = -17500, xend = "20_24", yend = -15000),
             size = 1.1, color = "#10002b",
             curvature = -0.5,
             arrow = arrow(length = unit(0.1, "in")))+
  #sexo feminino
  geom_text(aes(x="0_4", y=5000),
            label = "Feminino",
            hjust = 0,
            fontface = "bold"
  )+
  #sexo masculino
  geom_text(aes(x="0_4", y=-5000),
            label = "Masculino",
            hjust = 1,
            fontface = "bold"
  )+
  labs(x="Faixa etária",
       y="Vacinados",
       title = "Vacinados por faixa etária na cidade de Maringá-PR (até 14/09/21)",
       subtitle = "A porcentagem pode ser maior que 100% por haver mais vacinados que a população estimada pelo IPARDES para 2021",
       caption = "Gráfico: Bruno H. Mioto Stabile - @BrunoHMioto - Fonte: basedosdados.org (Vacinação), IPARDES (Estimativa popula??o 2021)") +
  scale_y_continuous(breaks = c(-20000, -15000, -10000, -5000, 0, 5000, 10000, 15000, 20000),
                     labels = c("-20000" = "20.000",
                                "-15000" = "15.000",
                                "-10000" = "10.000",
                                "-5000" = "5.000",
                                "0" = "0",
                                "5000" = "5.000",
                                "10000" = "10.000",
                                "15000" = "15.000",
                                "20000" = "20.000" )
  ) +
  scale_x_discrete(labels = c("0_4" = "0-4",
                              "5_9" = "5-9",
                              "10_14" = "10-14",
                              "15_19" = "15-19",
                              "20_24" = "20-24",
                              "25_29" = "25-29",
                              "30_34" = "30-34",
                              "35_39" = "35-39",
                              "40_44" = "40-44",
                              "45_49" = "45-49",
                              "50_54" = "50-54",
                              "55_59" = "55-59",
                              "60_64" = "60-64",
                              "65_69" = "65-69",
                              "70_74" = "70-74",
                              "75_79" = "75-79",
                              "80+" = "80+"),
                   position = "bottom") +
  
  coord_flip(ylim = c(-20000,20000)) + 
  scale_fill_manual(values = cols,
                    limits = c("População estimada (Masculino)",
                               "1ª Dose (M)",
                               "2ª Dose ou Única (M)",
                               "2ª Dose ou Única (F)",
                               "1ª Dose (F)",
                               "População estimada (Feminino)"))+ 
  guides(fill=guide_legend(nrow=1, byrow=TRUE))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.justification = "center",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    text = element_text(family = "Open Sans")
  )
