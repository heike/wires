#### Identify boundaries
#install.packages("concaveman")
library(concaveman)
library(x3ptools)
library(tidyverse)

x3p <- read_x3p("data/Aluminum Wires renamed/T1AW-LI-R4-B07.x3p")
x3p <- read_x3p("data/Aluminum Wires renamed/T1AW-LI-R2-B03.x3p")
x3p <- read_x3p("data/Aluminum Wires renamed/T1BW-LM-R1-B26.x3p")


boundary <- x3p %>% x3p_boundary_points(sample=10)

polygon_inside <- x3p_surface_polygon(boundary$x, boundary$y,
                                 concavity = 1.5,
                                 center = c(diff(range(boundary$x))/2,
                                            diff(range(boundary$y))/2))

boundary %>%
  ggplot(aes(x = x, y = y)) + geom_point() +
  geom_polygon(aes(x = x, y = y), fill = "orange",
               data = polygon_inside)

