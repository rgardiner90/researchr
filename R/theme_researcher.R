theme_researcher <- function(base_size = 10, font = NA) {
  txt <- element_text(size = base_size + 2)

  theme_classic(base_size = base_size, base_family = font) +
    theme(
      text = txt,
      plot.title = element_text(color = al_colors("main blue"), face = "bold"),
      plot.subtitle = element_text(face = "italic", color = al_colors("main light blue")),
      axis.title = txt,
      axis.text = txt,
      legend.title = txt,
      legend.text = txt,

      axis.line.y = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray70", size = 0.1),
      panel.grid.minor.y = element_line(color = "gray95", size = 0.1),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),

      legend.position = "bottom",

      plot.caption = element_text(hjust = 0)
    )
}



# ggplot(data = mtcars, aes(x = hp, y = mpg, color = factor(cyl))) +
#   geom_point(size = 3) +
#   labs(title = "test of theme", subtitle = "Is this italicized", caption = "where are you?") +
#   theme_researcher()
