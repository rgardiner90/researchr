#' Exploring biviate regression results of a dataframe
#'
#' This function returns either a graph or table of bivariate regression results for a dataset.
#' The results are helpful when trying to first gain an understanding of relationships between
#' a large number of variables.
#'
#' @param data A matrix, data frame, or tibble.
#' @param dependent A string character of the dependent variable
#' @param independent A string vector of the different independent variables to test
#' @param p_value The significance threshold for a table output (defaults to 0.05). Graph is
#' fixed at 95 percent confidence intervals
#' @param type Specifies the type of output request.  either 'graph' or 'table'
#' @param model_type Specificies whether you have an OLS or logit model.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' explore_bivariate(mtcars, "mpg", c("cyl", "disp", "hp", "drat"), p_value = 0.00001)
#' explore_bivariate(mtcars, names(mtcars)[1], names(mtcars)[-1])
explore_bivariate <- function(data, dependent, independent, p_value = 0.05, type = "graph",
                              model_type = "ols") {

  # getting the names of all the variables
  variables <- colnames(data)

  # getting the names of the independnet variables
  independent_text <- independent
  independent_variable <- data[, independent_text]

  # setting the dv
  dependent_text <- dependent
  dependent_variable <- data[, dependent_text]

  # setting the iv list
  iv_list <- independent_text

  # determining whether ols or logit
  modelType <- ifelse(tolower(model_type) == "ols", "gaussian",
                      ifelse(tolower(model_type) == "logit", "binomial",
                             "please select either 'ols' or 'logit'"))

  # running the models
  models <- lapply(iv_list, function(x) {
    broom::tidy(glm(substitute(dv ~ i, list(dv = as.name(dependent_text),
                                            i = as.name(x))),
                    data = data,
                    family = modelType))
  })


  # getting output
  if(type == "graph") {

    results <- models %>%
      tibble::tibble() %>%
      tidyr::unnest(cols = c(.)) %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::select(term, estimate, std.error) %>%
      dplyr::mutate(lower = (estimate - (std.error * 1.96)),
                    upper = (estimate + (std.error * 1.96)),
                    term = forcats::fct_reorder(term, estimate),
                    significance = ifelse(estimate > 0 & lower > 0, "positive",
                                          ifelse(estimate < 0 & upper < 0, "negative", "not significant"))) %>%
      ggplot2::ggplot(ggplot2::aes(x = term, y = estimate,
                                   ymin = lower, ymax = upper, color = significance)) +
      ggplot2::theme_minimal() +
      ggplot2::geom_hline(yintercept = 0.0, color = "red", lty = 2) +
      ggplot2::geom_point() +
      ggplot2::geom_linerange() +
      ggplot2::labs(title = "Results are from bivariate tests, not a single model.",
                    caption = "Graph results show a 95% confidence interval",
                    x = "", y = "Coefficient") +
      ggplot2::coord_flip() +
      ggplot2::scale_color_manual(values = c("red2", "gray", "#7CAE00"))

    return(results)

  } else if(type == "table") {

    results <- models %>%
      tibble::tibble() %>%
      tidyr::unnest(cols =c(.)) %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::mutate(significance = ifelse(p.value < p_value, 1, 0))

    return(results)
  } else {
    return("Please select either 'graph' or 'table' for type")
  }
}

