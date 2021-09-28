#' Logit model with k-fold cross validation
#'
#' Function for linear model and its results
#' @param data Name of the data frame.
#' @param formula Model in the format "y ~ x". Default: Predict variable y with all other variables.
#' @param k Number of kfolds. Default: 10.
#' @keywords linear model
#' @examples
#' kfold_logit(ISLR::Smarket, "Direction ~ .")
#' @export
#' @import dplyr
#' @importFrom modelr crossv_kfold
#' @importFrom purrr map map2_dbl map2
#' @importFrom broom tidy glance augment
#' @importFrom tidyr unnest
#' @importFrom stringi stri_trim_both
#' @importFrom pROC roc
#' @importFrom rlang .data

kfold_logit <- function(data, formula = "y ~ .", k = 10) {

  # Get predict variable from formula
  y <- stri_trim_both(unlist(strsplit(formula, split = "~"))[1])

  # Create test and train set and n folds
  models <- crossv_kfold(data, k = k) %>%
    mutate(
      model = map(.data$train, ~ glm(formula, family = binomial("logit"), data = .)),
      tidy = map(.data$model, tidy),
      glance = map(.data$model, glance))

  # Calculate coefficients
  coeffs <- models %>%
    unnest(.data$tidy) %>%
    select(.data$.id, .data$term, .data$estimate, .data$p.value) %>%
    group_by(.data$term) %>%
    summarise(estimate = mean(.data$estimate), p.value = mean(.data$p.value)) %>%
    arrange(.data$p.value)

  # Calculate model accuracy test set
  test_preds <- models %>%
    unnest(
      fitted = map2(.data$model, .data$test, ~augment(.x, newdata = .y)),
      pred = map2(.data$model, .data$test, ~predict(.x, .y, type = "response")))

  test_acc <- test_preds %>%
    group_by(.data$.id) %>%
    summarise(auc = roc(.data[[y]], .data$.fitted)$auc) %>%
    select(.data$auc) %>%
    pull %>%
    mean

  # Calculate model accuracy from train set
  train_preds <- models %>%
    unnest(
      fitted = map2(.data$model, .data$train, ~augment(.x, newdata = .y)),
      pred = map2(.data$model, .data$train, ~predict(.x, .y, type = "response")))

  train_acc <- train_preds %>%
    group_by(.data$.id) %>%
    summarise(auc = roc(.data[[y]], .data$.fitted)$auc) %>%
    select(.data$auc) %>%
    pull %>%
    mean

  # Return results as a list
  output <- list(
    test_acc = test_acc,
    train_acc = train_acc,
    coeffs = coeffs)

  return(output)
}
