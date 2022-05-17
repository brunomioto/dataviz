# packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)
library(ggnewscale)


# data --------------------------------------------------------------------

test <- data.frame(x = runif(1000),
                   y = runif(1000))


circles <- data.frame(
  x0 = 0.1,
  y0 = 0.9,
  r = 0.06
)

# plot --------------------------------------------------------------------

eye <- ggplot(test)+
  #fundo
  geom_tile(aes(x = 0.5, y=1, fill = y)) +
  scale_fill_gradient2(low = '#ffffff', mid = "#f3f1f4", high = '#e8e4e9',
                       midpoint = 0.6) +
  ggnewscale::new_scale_fill()+
  #iris fundo
  geom_tile(aes(x = 0.5, y=0.5, fill = y)) +
  scale_fill_gradient2(low = '#2b5a74', mid = '#08303c', high = '#08303c',
                       midpoint = 0.8) +
  #iris linha
  geom_line(aes(x,y,color = y))+
  scale_color_gradient(low = "#412808", high = "#ad8d6d30")+
  #contorno
  geom_hline(yintercept = 1.5, size = 1.2)+
  ggnewscale::new_scale_fill()+
  #pupila
  geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 0.4,
            fill = "#000000")+
  #reflexo
  geom_circle(data = circles, aes(x0 = x0, y0 = y0, r = r, fill = r),
              fill = "#ffffff")+
  scale_y_continuous(limits = c(0,1.5))+
  coord_polar()+
  theme_void()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )


ggsave(plot = eye, "eye.png", width = 6, height = 6)
