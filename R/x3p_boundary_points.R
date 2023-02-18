

x3p_boundary_points <- function(x, sample) {
  x3p_df <- x3p %>% x3p_sample(m=sample) %>% x3p_to_df()
  x_ranges <- x3p_df %>% group_by(y) %>% mutate(
    n = sum(!is.na(value))
  ) %>% filter(n > 0) %>%
    summarize(
      minx = min(x[!is.na(value)], na.rm=TRUE),
      maxx = max(x[!is.na(value)], na.rm=TRUE)
    )

  y_ranges <- x3p_df %>% group_by(x) %>% mutate(
    n = sum(!is.na(value))
  ) %>% filter(n > 0) %>%
    summarize(
      miny = min(y[!is.na(value)], na.rm=TRUE),
      maxy = max(y[!is.na(value)], na.rm=TRUE)
    )


  x_ranges %>%
    ggplot(aes(x = minx, y = y)) +
    geom_point(colour="blue") +
    geom_point(colour = "red", aes(x = maxx)) +
    geom_point(colour="orange", aes(x =x, y = miny), data = y_ranges) +
    geom_point(colour="green", aes(x =x, y = maxy), data = y_ranges) +
    #  geom_point(colour = "black", size = 0.5, aes(x = x, y = y), data = points) +
    geom_point(x = center[1], y = center[2], shape="x")


  points <- rbind(
    x_ranges %>% pivot_longer(minx:maxx, values_to = "x", names_to="whatever"),
    y_ranges %>% pivot_longer(miny:maxy, values_to = "y", names_to="whatever")
  ) %>% filter(!is.infinite(x), !is.infinite(y)) %>% select(-whatever)
}
