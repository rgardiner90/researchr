#' Checking the Gauss-Markov Assumptions
#' This function taks in an OLS model and returns a data frame of the
#' most common assumptions used to test a model. Used as a starting
#' point for testing the assumptions.  The results offer suggestions
#' about which assumptions deserve a closer examination.
#'
#' @param model_name An OLS model object.
#'
#'
#' @return Each row represents one assumption check. The three columns
#' show the test, the relevant value (depends on the exact test), and
#' an evaluation of whether further exploration is necessary. Normality,
#' constant variance, and autocorrelation are all significance tests.
#' Multicollinearity uses VIF scores (relevant only for multiple regression).
#' Conditional mean looks at the correlation between the residuals and the numeric
#' variable.  Currently, it is unable to properly evaluate the influence of a
#' categorical variable.
#'
#' @examples
#' model <- lm(dist ~ speed , data = cars) # running a model
#' check_assumptions(model)
#'
#' model2 <- lm(mpg ~ cyl + wt, data = mtcars)
#' check_assumptions(model2)
check_assumptions <- function(model_name) {

  ####
  # adds residuals
  ####
  resid <- residuals(model_name)

  ####
  # Most tests
  ####
  normality <- moments::jarque.test(resid) # residual test

  constant_variance <- car::ncvTest(model_name) # constant variance

  autocorrelation <- car::durbinWatsonTest(model_name) # autocorrelation


  # putting most tests together
  tests <- c("normality", "constant variance", "auto correlation")

  values <- c(signif(as.numeric(normality$p.value), 3),
              signif(as.numeric(constant_variance$p), 3), signif(as.numeric(autocorrelation$p), 3))

  problems <- ifelse(values < 0.05, "problem", "no problem")

  tests <- cbind(tests, values, problems)

  ####
  # adding multicollinearity
  ####
  viffy <- ifelse(length(model_name$coefficients) < 3,
                  "only one IV", max(car::vif(model_name)))

  viffy_results <- ifelse(viffy == "only one IV", "NA",
                          ifelse(viffy > 10, "problem",
                                 ifelse(viffy > 4, "potential problem", "no problem")))

  vif <- c("multicollinearity: vif", viffy, viffy_results)


  ####
  # adding conditional mean of the errors
  ####
  resid <- residuals(model_name)


  # running correlation between variables and residual (conditional mean of error is zero)
  correlation <- sapply(model_name$model,
                        function(x) ifelse(is.factor(x),
                                           max(abs(tapply(resid, x, mean))),
                                           cor(resid, x)))

  cor_problem <- ifelse(correlation > 0.2,
                        "potential problem, unkown if factor", "no problem, unknown if factor")

  cor_output <-cbind(paste0("conditional mean: ",
                            variable.names(model_name)), signif(correlation, 3), cor_problem)

  correlation_output <- cor_output[-1,]

  # creating the object to return
  testing <- data.frame(rbind(tests, vif, correlation_output))

  rownames(testing) <- c()

  # testing$values <- round(as.numeric(testing$values), 3)


  return(testing)
}
