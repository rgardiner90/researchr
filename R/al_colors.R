accesslex_colors <- c(`main blue` = "#002b49",
                `main light blue` = "#006072",
                `second yellow` = "#d18a00",
                `second orange` = "#d35e13",
                `second red` = "#8f1336",
                `second maroon` = "#632d4f",
                `third green` = "#556221",
                `third blue gray` = "#4b4f54",
                `third light blue` = "#7e9bc0")


al_colors <- function(...) {
  colors <- c(...)

  if(is.null(colors))
    return(accesslex_colors)

  accesslex_colors[colors]
}

# Examples -----------------

# al_colors()
#
# al_colors("main blue")
#
# al_colors("main blue", "second red")

# cars %>%
#   ggplot(aes(speed, dist)) +
#   geom_point(color = al_colors("second red"))
