#### Identify boundaries
#install.packages("concaveman")
library(x3ptools)
library(tidyverse)
library(wires)

x3p <- read_x3p("~/Documents/CSAFE/Wirecutter/data/Aluminum Wires renamed/T1AW-LI-R4-B07.x3p")
x3p <- read_x3p("~/Documents/CSAFE/Wirecutter/data/Aluminum Wires renamed/T1AW-LI-R2-B03.x3p")
x3p <- read_x3p("~/Documents/CSAFE/Wirecutter/data/Aluminum Wires renamed/T1BW-LM-R1-B26.x3p")


boundary <- x3p %>% x3p_boundary_points(sample=10)

polygon_inside <- x3p_surface_polygon(boundary$x, boundary$y,
                                 concavity = 1.5)

boundary %>%
  ggplot(aes(x = x, y = y)) + geom_point() +
  geom_polygon(aes(x = x, y = y), fill = "orange",
               data = polygon_inside)

# integer array;
# values are:
# 0: point is strictly exterior to pol;
# 1: point is strictly interior to pol;
# 2: point lies on the relative interior of an edge of pol;
# 3: point is a vertex of pol.
point.in.polygon(point.x, point.y, polygon_inside$x, polygon_inside$y, mode.checked=FALSE)


library(Cairo)
CairoPNG("mask.png",width = 2340, height=1725, units="px")
par(mar=c(0,0,0,0))
plot.default(x=c(1, 2340)*0.645, y=c(1, 1725)*0.645, type="n", frame.plot=FALSE, axes = FALSE)
polygon(rbind(polygon_inside[,c("x", "y")],polygon_inside[1,c("x", "y")]), col="red")
#savePlot("mask.png",device = dev.cur())
dev.off()
mask <- png::readPNG("mask.png")

x3p <- x3p_add_mask(x3p, mask=as.raster(mask))
image_x3p(x3p)
