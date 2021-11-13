# packages ----------------------------------------------------------------

library(tidyverse)
library(ggforce)
library(patchwork)

# data --------------------------------------------------------------------

col_1 <- c("a", "b", "c", "d")
col_2 <- c("e", "f", "g", "h")
col_3 <- c("i", "j", "k", "l")
value <- c(0, 5, 5, 5)

df <- data.frame(col_1, col_2, col_3, value)

df <- gather_set_data(df, 1:3)

df$y <- factor(df$y, levels = c("a", "b", "c", "d",
                                "f", "g", "h", "e",
                                "i", "j", "k", "l"
                                ))

# plot -------------------------------------------------

a <- df %>%
  ggplot(aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(
    fill = "black",
    axis.width = 0,
    sep = 0.5,
    strength = 0.7
  ) +
  geom_parallel_sets_axes(axis.width = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_void()

# copacabana ------------------------------------------

(a|a|a|a|a)/(a|a|a|a|a)

ggsave("copacabana3.png", width = 6, height = 6)

