#' Create descriptive statistics table
#'
#' Create descriptive statistics table
#' @param data Choose dataframe.
#' @param .select Choose which columns you want to calculate distribution.
#' @param .labels Choose labels for table. Default: total = "N"
#' @param with_idk Data includes I don't know values. You want to compute them in distributions but not in means. Default: 99.
#' @param group_by_cols Group by chosen columns. Defaul: NULL
#' @keywords social security, birth, date
#' @examples
#' desc_stat(mtcars, .select = c(2, 8:11), group_by_cols = "cyl")
#' @export
#' @import dplyr
#' @importFrom purrr reduce
#' @importFrom desctable desctable
#' @importFrom stringi stri_replace_all_regex
#' @importFrom rlang .data

desc_stat <- function(data, .select, group_by_cols = NULL,
                      with_idk = 99, .labels = c(total = "N")){

  # Need to bind dot to global variable so check() will pass
  . <- NULL

  # Add total to the first of the vector
  group_by_cols <- append(group_by_cols, "total", after = 0)

  # Create a list for the 1st loop and vector for N-values
  desc_list <- list()
  N <- c()

  # Loop to create a data grouped by my_group_by_cols variable
  for (i in 1:length(group_by_cols)) {
    data_num <- data %>%
      mutate(total = "total") %>%
      group_by_(group_by_cols[i]) %>%
      select(which(lapply(data, class) == "numeric"))

    # Remove with_idk numbers
    if (is.numeric(with_idk)) {
      data_num <- data_num %>%
        mutate_at(vars(-group_cols()), ~if_else(.x == with_idk, NA_real_, .x))
    }

    data_fct <- data %>%
      mutate_at(.select, as.factor) %>%
      mutate(total = "total") %>%
      group_by_(group_by_cols[i]) %>%
      select(.select)

    # Add N-values to a vector
    N <- append(N, group_size(data_num))

    # Create a list for the 2nd loop
    desc_list_grouped <- list()

    # Loop to get percentage and means for each group
    # TODO: remove at least this for loop by something nicer
    for (j in 1:length(unique(group_indices(data_num)))) {

      # Factor data
      desc_fct <- data_fct %>%
        group_indices() %in% j %>%
        data_fct[., ] %>%
        ungroup() %>%
        desctable(
          stats = list("Mean/ %" = is.factor ~ percent | (is.numeric ~ mean)),
          labels = .labels)

      names(desc_fct[[2]]) <- as.character(pull(group_keys(data_fct)[j,]))
      desc_fct <- bind_cols(lapply(desc_fct, as.data.frame)) %>%
        mutate(Variables = gsub("\\*","", .data$Variables))

      # Numeric data
      desc_num <- data_num %>%
        group_indices() %in% j %>%
        data_num[., ] %>%
        ungroup() %>%
        desctable(
          stats = list("Mean/ %" = is.factor ~ percent | (is.numeric ~ mean)),
          labels = .labels)

      names(desc_num[[2]]) <- as.character(pull(group_keys(data_num)[j,]))
      desc_num <- bind_cols(lapply(desc_num, as.data.frame))
      desc_comb <- full_join(
        desc_fct,
        desc_num,
        by = "Variables",
        suffix = c("",".y"))
      desc_comb[2] <- if_else(
        is.na(desc_comb[[2]]),
        desc_comb[[3]],
        desc_comb[[2]])
      desc_comb <- desc_comb[1:2]

      desc_list_grouped[j] <- list(desc_comb)
    }

    # Reduce lists as a dataframe by joining with Variable names
    desc_list_grouped_df <- desc_list_grouped %>%
      reduce(full_join, by = "Variables")


    # Create list for grouped descriptive stat dataframes
    desc_list[i] <- list(desc_list_grouped_df)
  }

  # Reduce lists as a dataframe by joining with Variable names
  output <- desc_list %>%
    reduce(left_join, by = "Variables")

  # Make final mutations to output like remove unnecessary characters and add N values to top
  #output <- output %>%
  #  mutate(Variables = stri_replace_all_regex(.data$Variables, ".*:\\s", "")) %>%
  #  mutate(Variables = stri_replace_all_regex(.data$Variables, "\\.", " "))

  output[1,2:ncol(output)] <- N

  # Return the data
  return(output)
}
