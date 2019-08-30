scale_fill_al <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- al_palette(palette = palette, reverse = reverse)

  if(discrete) {
    discrete_scale("fill", paste0("al_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}




# mtcars %>%
#   group_by(cyl) %>%
#   summarise(mpg = mean(mpg)) %>%
#   ggplot(aes(factor(cyl), mpg, fill = factor(cyl))) +
#   geom_col() +
#   scale_fill_al(palette = "three")
