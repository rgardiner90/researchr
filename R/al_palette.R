accesslex_palettes <- list(
  `main` = al_colors("main light blue", "second yellow", "third blue gray"),
  `three` = al_colors("main blue", "second orange", "third green"),
  `five` = al_colors("main light blue", "second maroon", "third green", "second orange", "third light blue"),
  `seven` = al_colors("main blue", "second red", "main light blue", "second yellow", "third green", "second orange",
                      "third blue"),
  `nine` = al_colors(),
  `diverging` = al_colors("third light blue", "second orange")
)

al_palette <- function(palette = "main", reverse = FALSE, ...) {
  pal <- accesslex_palettes[[palette]]

  if(reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}


# al_palette("five")
#
# al_palette("diverging")
# al_palette("diverging")(10)
