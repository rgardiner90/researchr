scale_color_al <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- al_palette(palette = palette, reverse = reverse)

  if(discrete) {
    discrete_scale("colour", paste0("al_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}





# ggplot(mtcars, aes(hp, mpg, color = factor(cyl))) +
#   geom_point(size = 3) +
#   labs(title = "test of theme", subtitle = "Is this italicized", caption = "where are you?") +
#   scale_color_al()


# mtcars %>%
#   ggplot(aes(hp, mpg, color = wt)) +
#   geom_point(size = 4) +
#   scale_color_al(discrete = FALSE, palette = "diverging")
