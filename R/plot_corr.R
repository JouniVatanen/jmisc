#' Custom correlation plot
#'
#' Make a custom correlation plot.
#' @param data Choose the dataframe.
#' @param polychoric Not yet implemented. TRUE = use polychoric correlations.
#' @keywords plot
#' @examples
#' plot_corr(mtcars)
#' @export
#' @import dplyr ggplot2
#' @importFrom tidyr gather
#' @importFrom stats hclust as.dist cor
#' @importFrom rlang .data

plot_corr <- function(data, polychoric = FALSE) {

  # Shorten variable names to 30 characters
  names(data) <- substring(names(data), 1, 30)

  # Create correlation matrix
  # TODO: if polychoric = TRUE, then calculate polychoric correlations
  corr_m <- cor(data, use = "pairwise.complete.obs")

  # Reorder correlation matrix
  dd <- as.dist(1 - corr_m / 2)
  hc <- hclust(dd)
  corr_m <- round(corr_m[hc$order, hc$order], 2)

 # Create output
  output <- as.data.frame(corr_m) %>%

    # Mutate correlation matrix
    mutate(Var1 = factor(row.names(corr_m), levels = row.names(corr_m))) %>%
    gather(key = "Var2", value = "value", -.data$Var1,
           na.rm = TRUE, factor_key = TRUE) %>%

    # Plot correlation matrix
    ggplot(aes(.data$Var2, .data$Var1, fill = .data$value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "blue", high = "red", mid = "white",
        midpoint = 0, limit = c(-1,1), space = "Lab") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      coord_fixed()

  # Return plot
  return(output)
}
