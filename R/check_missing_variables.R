#' Checking a dataset for missing observations across variables
#'
#' This function returns a graph that shows variable missingness across the
#' entire dataset.
#'
#' @param dataset A vector, matrix, data frame, or tibble.
#'
#' @importFrom magrittr %>%
#'
#' @return If there is no missing data the function will return a string
#' informing you that there is no missing data. Otherwise a graph will
#' appear showing the variable one the y axis and the percent missing on the x-axis.
#'
#' @export
#'
#' @examples
#' check_missing_variables(cars) # no missing data
#' check_missing_variables(airquality) # shows missing data
check_missing_variables <- function(dataset) {

  column_missing_data <- sapply(dataset, function(x) sum(is.na(x)))

  columns <- colnames(dataset)

  variable_missing_data <- as.data.frame(cbind(as.numeric(column_missing_data),
                                               columns))

  variable_missing_data$num_rows_NA <- as.numeric(as.character(variable_missing_data$V1))

  number_of_variables <- variable_missing_data[variable_missing_data$V1 != 0, ]

  if(all(variable_missing_data[, 1] == 0)) {
    return("Yay! You have no missing data")
  } else if(nrow(number_of_variables) > 10) {
    variable_missing_data %>%
      dplyr::mutate(proportion_missing = num_rows_NA / nrow(dataset),
                    percent_missing = (proportion_missing * 100)) %>%
      dplyr::arrange(desc(percent_missing)) %>%
      dplyr::filter(percent_missing != 0) %>%
      dplyr::mutate(columns = reorder(columns, percent_missing)) %>%
      ggplot2::ggplot(ggplot2::aes(factor(columns), percent_missing, fill = percent_missing)) +
      ggplot2::geom_col()  +
      ggplot2::labs(x = "Original Variables", y ="Percentage of Observations that are Missing",
                    fill = "Percent Missing") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient2(limits = c(0, 100), low = "light blue", high = "dark red", mid = "light blue")
  } else {
    variable_missing_data %>%
      dplyr::mutate(proportion_missing = num_rows_NA / nrow(dataset),
                    percent_missing = (proportion_missing * 100),
                    missing = paste(V1, "Missing \nObservations")) %>%
      dplyr::arrange(desc(percent_missing)) %>%
      dplyr::filter(percent_missing != 0) %>%
      dplyr::mutate(columns = reorder(columns, percent_missing)) %>%
      ggplot2::ggplot(ggplot2::aes(factor(columns), percent_missing, fill = percent_missing,
                                   label = missing)) +
      ggplot2::geom_col()  +
      ggplot2::labs(x = "Original Variables", y ="Percentage of Observations that are Missing",
                    fill = "Percent Missing") +
      ggplot2::coord_flip() +
      ggplot2::geom_text(hjust = 1.3) +
      ggplot2::scale_fill_gradient2(limits = c(0, 100), low = "light blue", high = "dark red", mid = "light blue")
  }
}
