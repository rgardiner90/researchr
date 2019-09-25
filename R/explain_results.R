#' Explaining Results for OLS models
#'
#' This function returns a string for a specific variable that explains in simple terms
#' the influence of the independent variable on the dependent variable.
#'
#' @param model_name The oLS model created
#' @param variable The variable (aka term) used in the model. Needs to be exactly how tidy() would
#' name the variable.
#' @param p_value The significance threshold used to determine significance
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom broom tidy
#' @importFrom tibble as_tibble
#' @importFrom tidyr spread
#' @importFrom tidyr unite
#'
#' @export
#'
#' @examples
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' model2 <- lm(mpg ~ wt + cyl + hp + wt + disp, data = mtcars)
#' explain_results(model1, "wt")
#' explain_results(model2, "cyl", p_value = 0.1)
explain_results <- function(model_name, variable, p_value = 0.05) {

  # extracting results
  results <- broom::tidy(model_name) %>%
    dplyr::mutate(directionality1 = ifelse(estimate < 0, "negative", "positive"),
                  directionality2 = ifelse(estimate < 0, "decreased", "increased"),
                  absolute_value = round(abs(estimate), 3)) %>%
    dplyr::filter(term == variable)

  # dependent variable
  dv <- names(model_name$model)[1]

  # independent variables
  independent_variables <- names(model_name$model)[-1]





  # output

  if(results$p.value > p_value) {
    return(paste0("The variable '", results$term, "' is not significantly related to '",
                  dv, "'."))
  } else if(length(independent_variables) < 2) {
    return(paste0("The variable '", results$term, "' had a ", results$directionality1,
                  " relationship with '", dv,
                  "'. Where when '", results$term, "' went up by 1 unit, '", dv, "' ",
                  results$directionality2, " by ", results$absolute_value))

  } else {

    # controls
    controls <- independent_variables %>%
      tibble::as_tibble() %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      dplyr::filter(value != variable) %>% # interactive part
      tidyr::spread(row_id, value) %>%
      tidyr::unite("ivs", 1:ncol(.), sep = ", ")


    return(paste0("The variable '", results$term, "' had a ", results$directionality1,
                  " relationship with '", dv,
                  "'. Where when '", results$term, "' went up by 1 unit, '", dv, "' ",
                  results$directionality2, " by ", results$absolute_value,
                  " while controlling for: ", controls))
    # this last line spits out multiple rows.  That is a problem

  }
}
