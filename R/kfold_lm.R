#' Linear model with k-fold cross validation
#'
#' Function for linear model and its results
#' @param data Name of the data frame.
#' @param formula Model in the format "y ~ x". Default: Predict variable y with all other variables.
#' @param k Number of kfolds. Default: 10.
#' @keywords linear model
#' @examples
#' kfold_lm(mtcars, "mpg ~ .")
#' @export
#' @import dplyr ggplot2
#' @importFrom modelr crossv_kfold rmse
#' @importFrom purrr map map2_dbl map2
#' @importFrom broom tidy glance augment
#' @importFrom tidyr unnest
#' @importFrom stringi stri_trim_both
#' @importFrom rlang .data

kfold_lm <- function(data, formula = "y ~ .", k = 10) {

  # Get predict variable from formula
  y <- stri_trim_both(unlist(strsplit(formula, split = "~"))[1])

  # Names for lm plots
  name_x_axis <- "Summamuuttuja"
  name_y_axis <- "Virhetermi"
  name_title_test <- "Testiaineisto"
  name_title_train <- "Mallinnusaineisto"

  # Create test and train set and n folds
  models <- crossv_kfold(data, k = k) %>%
    mutate(
      model = map(.data$train, ~lm(formula, data = .)),
      tidy = map(.data$model, tidy),
      glance = map(.data$model, glance),
      test_rmse = map2_dbl(.data$model, .data$test, rmse),
      train_rmse = map2_dbl(.data$model, .data$train, rmse))

  # Calculate coefficients
  coeffs <- models %>%
    unnest(tidy) %>%
    select(.data$.id, .data$term, .data$estimate, .data$p.value) %>%
    group_by(.data$term) %>%
    summarise(
      estimate = mean(.data$estimate),
      p.value = mean(.data$p.value)) %>%
    arrange(.data$p.value)

  # Calculate R^2
  r2 <- models %>% unnest(glance) %>% pull(.data$adj.r.squared) %>% mean

  # Calculate model accuracy test set
  test_preds <- models %>%
    unnest(fitted = map2(.data$model, .data$test, ~augment(.x, newdata = .y))) %>%
    mutate(.fitted = pmax(3, pmin(12, .data$.fitted)), .resid = .data$.fitted - .data[[y]])
  test_acc <- models %>% select(.data$test_rmse) %>% pull %>% mean

  # Plot residuals
  test_res_plot <- test_preds %>%
    ggplot(aes(.data[[y]], .data$.resid)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    stat_smooth(method = "loess") +
    theme_minimal() +
    labs(y = name_y_axis, x = name_x_axis, title = name_title_test)

  # Calculate model accuracy train set
  train_preds <- models %>%
    unnest(fitted = map2(.data$model, .data$train, ~augment(.x, newdata = .y))) %>%
    mutate(.fitted = pmax(3, pmin(12, .data$.fitted)), .resid = .data$.fitted - .data[[y]])

  train_acc <- models %>% select(.data$train_rmse) %>% pull %>% mean

  # Plot residuals
  train_res_plot <- train_preds %>%
    ggplot(aes(.data[[y]], .data$.resid)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    stat_smooth(method = "loess") +
    theme_minimal() +
    labs(y = name_y_axis, x = name_x_axis, title = name_title_train)

  # Return results as a list
  output <- list(
    coeffs = coeffs,
    r2 = r2,
    test_acc = test_acc,
    test_res_plot = test_res_plot,
    train_acc = train_acc,
    train_res_plot = train_res_plot)

  return(output)
}
