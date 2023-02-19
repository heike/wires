#' Identify boundary of a 3d topographic scan in x3p format
#'
#' @param x3p topographic scan in x3p format
#' @param sample positive integer value specifying the sampling ratio:
#' every `sample` value in x and y direction will be included.
#' Higher values result in faster results but cruder assessments of the boundary.
#' @return data frame of boundary points, variables are named `x` and `y`
#' @importFrom x3ptools x3p_sample x3p_to_df
#' @importFrom dplyr `%>%` group_by mutate filter summarize select
#' @importFrom rlang .data
#' @export
#' @examples
#' library(x3ptools)
#' x3p <- x3p_read(system.file("T1AW-LI-R4.x3p", package="wires"))
#' bounds <- x3p_boundary_points(x3p, 2)
#'
#' library(ggplot2)
#' library(dplyr)
#' bounds %>% ggplot(aes(x = x, y = y)) + geom_point()
x3p_boundary_points <- function(x3p, sample) {
  stopifnot('x3p' %in% class(x3p))
  x3p_df <- x3p %>% x3p_sample(m=sample) %>% x3p_to_df()
  x_ranges <- x3p_df %>% group_by(.data$y) %>% mutate(
    n = sum(!is.na(.data$value))
  ) %>% filter(.data$n > 0) %>%
    summarize(
      minx = min(.data$x[!is.na(.data$value)], na.rm=TRUE),
      maxx = max(.data$x[!is.na(.data$value)], na.rm=TRUE)
    )

  y_ranges <- x3p_df %>% group_by(.data$x) %>% mutate(
    n = sum(!is.na(.data$value))
  ) %>% filter(.data$n > 0) %>%
    summarize(
      miny = min(.data$y[!is.na(.data$value)], na.rm=TRUE),
      maxy = max(.data$y[!is.na(.data$value)], na.rm=TRUE)
    )


  # x_ranges %>%
  #   ggplot(aes(x = minx, y = y)) +
  #   geom_point(colour="blue") +
  #   geom_point(colour = "red", aes(x = maxx)) +
  #   geom_point(colour="orange", aes(x =x, y = miny), data = y_ranges) +
  #   geom_point(colour="green", aes(x =x, y = maxy), data = y_ranges) +
  #   #  geom_point(colour = "black", size = 0.5, aes(x = x, y = y), data = points) +
  #   geom_point(x = center[1], y = center[2], shape="x")


  points <- rbind(
    x_ranges %>% pivot_longer(.data$minx:.data$maxx, values_to = "x", names_to="whatever"),
    y_ranges %>% pivot_longer(.data$miny:.data$maxy, values_to = "y", names_to="whatever")
  ) %>% filter(!is.infinite(.data$x), !is.infinite(.data$y)) %>% select(-.data$whatever)
}
