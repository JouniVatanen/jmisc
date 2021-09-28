#' plot_missing
#'
#' This function helps you to ggplot missing values of a data frame.
#' @param data Enter your data frame.
#' @param file Output file, if you want to save the plot.
#' @keywords ggplot2 missing
#' @examples
#' mtcars[10:17, 2:5] <- NA
#' mtcars[3:5, 8:9] <- NA
#' plot_missing(mtcars)
#' @export
#' @import ggplot2 dplyr
#' @importFrom tidyr gather

plot_missing <- function(data, file = NULL) {

  # Shorten variable names to 30 characters
  names(data) <- substring(names(data), 1, 30)

  # Create missmap dataframe
  data <- as.data.frame(is.na(data))

  plot <- data %>%
    mutate(Var1 = factor(rownames(data), levels = rownames(data))) %>%
    gather(key = "Var2", value = "value", -.data$Var1,
           na.rm = TRUE, factor_key = TRUE) %>%

  # Plot missing values
    ggplot(aes(x = .data$Var2, y = .data$Var1)) +
      geom_raster(aes(fill = .data$value)) +
      scale_fill_grey(name = '', labels = c('Present', 'Missing')) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(x = 'Variables in data', y= 'Rows/observations')

  if (!is.null(file)) {
    # Save plot to png
    ggsave(file, plot, width = 20, height = 20)
  } else {
    return(plot)
  }
}
