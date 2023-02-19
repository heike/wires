#' Add a polygon of the scan shape as mask
#'
#' @param x3p x3p file
#' @param colour colour for the polygon
#' @param center point on the center of the scan. If NULL, a center will be
#' derived from the boundary points
#' @param concavity strictly positive value used in `concaveman::concaveman`
#' @return x3p object with mask
#' @importFrom Cairo CairoPNG
#' @importFrom x3ptools x3p_get_scale x3p_add_mask
#' @importFrom png readPNG
#' @importFrom grDevices as.raster dev.off
#' @importFrom graphics par plot.default polygon
#' @export
#' @examples
#' if (interactive()) {
#'   library(x3ptools)
#'   x3p <- x3p_read(system.file("T1AW-LI-R4.x3p", package="wires"))
#'   x3p <- x3p %>% x3p_surface_polygon()
#'   x3p_image(x3p, size = dim(x3p$surface.matrix), zoom=.6)
#' }
x3p_surface_polygon <- function(x3p, colour = "red", center = NULL, concavity = 1.5) {
  stopifnot("x3p" %in% class(x3p), is.numeric(concavity), concavity > 0)

  boundary <- x3p %>% x3p_boundary_points(sample = 10) # we might have to make sample a parameter
  polygon_inside <- inside_polygon(boundary$x, boundary$y,
                                   concavity = concavity,
                                   center = center)
  dims <- dim(x3p$surface.matrix)
  resolution <- x3p %>% x3p_get_scale()

  mask_png <- tempfile(fileext = "png")

  CairoPNG(mask_png,width = dims[1], height=dims[2], units="px")
  par(mar=c(0,0,0,0))
  plot.default(x=c(1, dims[1])*resolution, y=c(1, dims[2])*resolution, type="n", frame.plot=FALSE, axes = FALSE)
  polygon(rbind(polygon_inside[,c("x", "y")],polygon_inside[1,c("x", "y")]), col=colour)
#savePlot("mask.png",device = dev.cur())
  dev.off()
  mask <- png::readPNG(mask_png)

  x3p_add_mask(x3p, mask=as.raster(mask))
}
