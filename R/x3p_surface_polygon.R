

inside_polygon <- function(x, y, concavity, center) {

  points_inside_out <- data.frame(x, y) %>%
    mutate(x = x-center[1], y = y-center[2]) %>%
    mutate(
      r = sqrt(x^2+y^2),
      theta = atan(y/x),
      theta = ifelse(x < 0, atan(y/x) + pi, ifelse(y < 0, theta+2*pi, theta))
    ) %>%
    mutate(
      xout = 1/r * cos(theta),
      yout  = 1/r * sin(theta)
    )

  points_mat_left <- points_inside_out %>% select(xout, yout) %>%
    mutate(xout = xout) %>% as.matrix()
  polygons_left <- concaveman(points_mat_left, concavity = concavity)

  points_mat_right <- points_inside_out %>% select(xout, yout) %>%
    mutate(xout = -xout) %>% as.matrix()
  polygons_right <- concaveman(points_mat_right, concavity = concavity)

  polygon_inside_out <- rbind(
    data.frame(polygons_left) %>% filter(V1 < 0),
    data.frame(polygons_right) %>% filter(V1 < 0) %>% mutate(V1 = -V1)
  )

  polygon_inside_out <- polygon_inside_out %>%
    rename(xout=V1, yout=V2)

  polygon_inside_out <- polygon_inside_out %>%
    mutate(
      rout = sqrt(xout^2+yout^2),
      theta = atan(yout/xout),
      theta = ifelse(xout < 0, atan(yout/xout) + pi, ifelse(yout < 0, theta+2*pi, theta))
    ) %>% mutate(
      x = 1/rout * cos(theta),
      y = 1/rout * sin(theta)
    )
  polygon_inside_out <- polygon_inside_out %>% arrange(theta) %>% mutate(id=1:n())
  polygon_inside_out %>% mutate(
    x = x + center[1],
    y = y + center[2]
  )
}

