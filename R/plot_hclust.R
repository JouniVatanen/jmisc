#' Custom dendrogram plot
#'
#' Make a custom dendrogram plot.
#' @param data Choose the dataframe.
#' @param cut_num Choose the number where to cut the number of clusters. Default: 5.
#' @param clust_var Choose if you want to cluster variables. Default: TRUE.
#' @param horiz Plot horizontal (TRUE) or vertical (FALSE). Default: FALSE.
#' @param labels_track_height Make extra space for variable names. Default: 120.
#' @param main Title for the plot.
#' @param ... Pass other parameters to fviz_dend like type, cex, ggtheme.
#' @keywords data dendrogram cluster
#' @examples
#' plot_hclust(mtcars, cut_num = 4, horiz = TRUE, labels_track_height = 1)
#' @export
#' @import dplyr
#' @importFrom ClustOfVar hclustvar
#' @importFrom factoextra fviz_dend
#' @importFrom stats dist hclust

plot_hclust <- function(data, cut_num = 5, clust_var = TRUE, horiz = FALSE,
                        labels_track_height = 120, main = "", ...) {

  # ClustofVar if clust_var = TRUE
  if (clust_var) {

    # Select numeric variables and NULL if no variables
    var_quanti <- data %>%
      select_if(is.numeric)

    # Select non-numeric variables and convert them to factors and NULL if no variables
    var_quali <- data %>%
      select_if(Negate(is.numeric)) %>%
      mutate_all(as.factor)

    if (is.null(dim(var_quanti)) | dim(var_quanti)[2] == 0) {var_quanti <- NULL}
    if (is.null(dim(var_quali)) | dim(var_quali)[2] == 0) {var_quali <- NULL}

    # Variable hierarchical cluster
    hc <- hclustvar(X.quanti = var_quanti, X.quali = var_quali)

  } else {
    # Else normal hierarchical cluster
    hc <- hclust(dist(data))
  }

  # Choose custom colors, where rep_len matches number of clusters
  colors <- rep_len(brand_cols(), cut_num)

  # Plot the clustering
  plot <- fviz_dend(
      hc, k = cut_num, cex = 0.5, horiz = horiz, k_colors = colors,
      color_labels_by_k = TRUE, rect = TRUE, rect_border = colors,
      rect_fill = TRUE, labels_track_height = labels_track_height,
      main = main, ...) +
    theme_void()

  return(plot)
}
