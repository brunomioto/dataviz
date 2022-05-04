# packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readr)
library(ggfx)

# data --------------------------------------------------------------------

data <- read_csv("characters.csv")


list_char <- c("Luke Skywalker",
               "Darth Vader",
               "Obi-Wan Kenobi",
               "Yoda",
               "Mace Windu",
               "Leia Organa")

data2 <- data %>%
  mutate(height = height/100)


# plot --------------------------------------------------------------------

plot <- data2 %>% 
  ggplot(aes(x = height, y = height))+
    #Yoda
    with_outer_glow(
      geom_segment(data=filter(data2,name == "Yoda"),
                   aes(x = 0, xend = height, yend = name, y = name),
                   color = "#ffffff",
                   size = 3,
                   lineend = "round"),
      colour = "#02fe2c",
      sigma = 20,
      expand = 10
    )+ 
    #Leia
    with_outer_glow(
      geom_segment(data=filter(data2,name == "Leia Organa"),
                   aes(x = 0, xend = height, yend = name, y = name),
                   color = "#ffffff",
                   size = 3,
                   lineend = "round"),
      colour = "#006be4",
      sigma = 20,
      expand = 10
    )+ 
    #luke
    with_outer_glow(
      geom_segment(data=filter(data2,name == "Luke Skywalker"),
                   aes(x = 0, xend = height, yend = name, y = name),
                   color = "#ffffff",
                   size = 3,
                   lineend = "round"),
      colour = "#02fe2c",
      sigma = 20,
      expand = 10
    )+
    #Obi-Wan Kenobi
    with_outer_glow(
      geom_segment(data=filter(data2,name == "Obi-Wan Kenobi"),
                   aes(x = 0, xend = height, yend = name, y = name),
                   color = "#ffffff",
                   size = 3,
                   lineend = "round"),
      colour = "#006be4",
      sigma = 20,
      expand = 10
    )+ 
    #Mace Windu
    with_outer_glow(
      geom_segment(data=filter(data2,name == "Mace Windu"),
                   aes(x = 0, xend = height, yend = name, y = name),
                   color = "#ffffff",
                   size = 3,
                   lineend = "round"),
      colour = "#d413ef",
      sigma = 20
    )+
    #darth vader
    with_outer_glow(
      geom_segment(data=filter(data2,name == "Darth Vader"),
                   aes(x = 0, xend = height, yend = name, y = name),
                   color = "#ffffff",
                   size = 3,
                   lineend = "round"),
      colour = "#e00301",
      sigma = 30,
      expand = 10
    )+
    labs(
      title = "Height of some Star Wars characters",
      x = "Height, m",
      caption = "Bruno Mioto - @BrunoHMioto"
    )+
    scale_x_continuous(expand = expansion(mult = c(0,0.05)))+
    scale_y_discrete(limits = c("Yoda",
                                "Leia Organa",
                                "Luke Skywalker",
                                "Obi-Wan Kenobi",
                                "Mace Windu",
                                "Darth Vader"))+
    theme_minimal()+
    theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      panel.grid.major.y = element_line(linetype = "dashed", color = "grey"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(size = rel(1.5)),
      text = element_text(color = "white", face = "bold"),
      axis.text = element_text(color = "white"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)
    )+
  coord_flip()


ggsave(plot = plot, "starwars_height_flip.png", height = 10, width = 5)
