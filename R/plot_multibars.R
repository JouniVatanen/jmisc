#' Multiple custom bar plots
#'
#' Create a multiple custom bar plots. With purr::map you can create a list of barplots.
#' @param data dataframe
#' @param row select which row to plot. Default: 1:nrow(.)
#' @param labels select labels. Default: waiver()
#' @keywords plot ggplot2 multiplot
#' @examples
#' # Plot multiple bar plots to same grid
#' library(gridExtra)
#'
#' plot_basic <- mtcars %>%
#'   dplyr::group_by(cyl) %>%
#'   dplyr::summarise_at(dplyr::vars(mpg, disp, hp, wt), ~mean(.)) %>%
#'   tidyr::gather(2:ncol(.), key = "question", value = "values") %>%
#'   tidyr::spread(cyl, values) %>%
#'   {purrr::map(list(1, 2, 3), function(x) plot_multibars(., x))}
#' do.call("grid.arrange", c(plot_basic, ncol = 2))
#'
#' # Plot a single bar plot
#' plot_multibars(mtcars, row = 3)
#' @export
#' @import dplyr ggplot2
#' @importFrom tidyr gather
#' @importFrom purrr map
#' @importFrom gridExtra grid.arrange


plot_multibars <- function(data, row = 1:nrow(data), labels = waiver()) {

  data %>%

    # Choose which row to plot
    slice(row) %>%

    # Gather many columns to ggplot format
    gather(2:ncol(data), key = "question", value = "values") %>%

    # Plot data
    ggplot(aes(x = .data$question, y = .data$values, fill = .data$question,
               label = round(.data$values, 1))) +
      geom_col(position = "dodge") +
      geom_text(position = position_dodge(width = 1), vjust = 0) +

      # Choose Brand colors
      scale_fill_brand() +

      # Choose x-axis labels, if necessary
      scale_x_discrete(labels = labels) +

      # Choose theme
      theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")
}
