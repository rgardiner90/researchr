#' Creating predictions using simulated data
#'
#' This function returns a data frame of predictions using simulated data.
#' This result only works on OLS or logit models.
#'
#' @param model The model used to create the predictiosn
#' @param dataset The underlying dataset that is used to create the underlying
#' values of the predictor variable
#' @param ... List of predictor variables that are being varied.  All other variables are set to 'typical' values
#' @param model_type Specifies whether you have an 'ols' or 'logit' model
#'
#' @importFrom magrittr %>%
#' @importFrom modelr data_grid
#' @importFrom modelr add_predictions
#' @importFrom dplyr select
#' @importFrom dplyr quos
#'
#' @export
#'
#' @example
#' model <- lm(mpg ~ ., data = mtcars)
#' create_predictions(model, mtcars, cyl, wt)
#' create_predictions(model, mtcars, gear)
create_predictions <- function(model, dataset, ..., model_type = "ols") {

  variable <- dplyr::quos(...)

  if (tolower(model_type) == "ols") {
  dataset %>%
    modelr::data_grid(!!!variable, .model = model) %>%
    na.omit() %>%
    modelr::add_predictions(model) %>%
    dplyr::select(!!!variable, pred)
  } else if(tolower(model_type) == "logit") {
    dataset %>%
      modelr::data_grid(!!!variable, .model = model) %>%
      na.omit() %>%
      modelr::add_predictions(model, type = "response") %>%
      dplyr::select(!!!variable, pred)
  } else {
    return("please select either 'ols' or 'logit'")
  }

}
