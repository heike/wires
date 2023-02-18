#' Identify the inside surface area of a set of points
#'
#' @param x numeric vector of x positions
#' @param y numeric vector of y positions
#' @param concavity strictly positive parameter. Smaller values approach the
#' inside of the boundary
#' more closely. Large values concentrate on the inner center of the area
#' @param center numeric vector of length 2, consisting of (x,y) coordinate for
#' centering x and y values.
#' If center is NULL, the half ranges of x and y are used.
#' @return dataframe of x and y positions describing the inside of the area
#' described by the input x and y, variable id describes the order of the points
#' @importFrom dplyr mutate select rename arrange n
#' @importFrom concaveman concaveman
#' @importFrom tidyr pivot_longer
#' @export
#' @examples
#' library(x3ptools)
#' x3p <- x3p_read("~/Documents/CSAFE/Wirecutter/data/Aluminum Wires renamed/T1AW-LI-R4-B07.x3p")
#' bounds <- x3p_boundary_points(x3p, 10)
#' polygon <- x3p_surface_polygon(bounds$x, bounds$y, 1)
#'
#' library(ggplot2)
#' library(dplyr)
#' bounds %>%
#' ggplot(aes(x = x, y = y)) + geom_point() +
#'   geom_polygon(data = polygon)
#'
x3p_surface_polygon <- function(x, y, concavity, center = NULL) {
  stopifnot(concavity > 0)

  if (is.null(center)) {
    center <- c(diff(range(x, na.rm=TRUE)), diff(range(y, na.rm=TRUE)))/2
  }
  if (length(center) == 1) center = rep(center, 2)
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(center), length(center)==2)

  points_inside_out <- data.frame(x, y) %>%
    mutate(x = x-center[1], y = y-center[2]) %>%
    mutate(
      r = sqrt(x^2+y^2),
      theta = atan(y/x),
      theta = ifelse(x < 0, atan(y/x) + pi, ifelse(y < 0, .data$theta+2*pi, .data$theta))
    ) %>%
    mutate(
      xout = 1/.data$r * cos(.data$theta),
      yout  = 1/.data$r * sin(.data$theta)
    )

  points_mat_left <- points_inside_out %>% select(.data$xout, .data$yout) %>%
    as.matrix()
  polygons_left <- concaveman(points_mat_left, concavity = concavity)

  points_mat_right <- points_inside_out %>% select(.data$xout, .data$yout) %>%
    mutate(xout = -.data$xout) %>% as.matrix()
  polygons_right <- concaveman(points_mat_right, concavity = concavity)

  polygon_inside_out <- rbind(
    data.frame(polygons_left) %>% filter(.data$V1 < 0),
    data.frame(polygons_right) %>% filter(.data$V1 < 0) %>% mutate(V1 = -.data$V1)
  )

  polygon_inside_out <- polygon_inside_out %>%
    rename(xout=.data$V1, yout=.data$V2)

  polygon_inside_out <- polygon_inside_out %>%
    mutate(
      rout = sqrt(.data$xout^2+.data$yout^2),
      theta = atan(.data$yout/.data$xout),
      theta = ifelse(.data$xout < 0, .data$theta + pi,
                     ifelse(.data$yout < 0, .data$theta+2*pi, .data$theta))
    ) %>% mutate(
      x = 1/.data$rout * cos(.data$theta),
      y = 1/.data$rout * sin(.data$theta)
    )
  polygon_inside_out <- polygon_inside_out %>%
    arrange(.data$theta) %>% mutate(id=1:n())
  polygon_inside_out %>% mutate(
    x = .data$x + center[1],
    y = .data$y + center[2]
  )
}

