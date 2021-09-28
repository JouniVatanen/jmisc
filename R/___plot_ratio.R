#' plot_ratio NOT YET FINISHED SO NOT INCLUDED IN PACKAGE
#'
#' GGally custom my_ratio plot for a large ... plot.
#' @param
#' @keywords GGally ggplot
#' @import ggplot GGally
#' @importFrom plyr count

my_ratio <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping)
}

ggally_ratio <- function(
  data,
  mapping = do.call(aes_string, as.list(colnames(data)[1:2])),
  ...,
  floor = 0,
  ceiling = NULL
) {

  # capture the original names
  xName <- mapping_string(mapping$x)
  yName <- mapping_string(mapping$y)
  countData <- count(data, vars = c(xName, yName))

  # overwrite names so name clashes don"t happen
  colnames(countData)[1:2] <- c("x", "y")
  xNames <- levels(countData[["x"]])
  yNames <- levels(countData[["y"]])

  countData <- subset(countData, freq >= floor)

  if (is.null(ceiling)) {
    ceiling <- max(countData$freq)
  }

  countData[["freqSize"]] <- sqrt(pmin(countData[["freq"]], ceiling) / ceiling)
  countData[["col"]] <- ifelse(countData[["freq"]] > ceiling, "grey30", "grey50")

  countData[["xPos"]] <- as.numeric(countData[["x"]]) + (1 / 2) * countData[["freqSize"]]
  countData[["yPos"]] <- as.numeric(countData[["y"]]) + (1 / 2) * countData[["freqSize"]]

  p <- ggplot(
    data = countData,
    mapping = aes_string(
      x = "xPos",
      y = "yPos",
      height = "freqSize",
      width = "freqSize",
      fill = "col"
    )
  ) +
    geom_tile(...) +
    scale_fill_identity() +
    scale_x_continuous(
      name = xName,
      limits = c(0.9999, length(xNames) + 1),
      breaks = 1:(length(xNames) + 1),
      labels = c(xNames, ""),
      minor_breaks = FALSE
    ) +
    scale_y_continuous(
      name = yName,
      limits = c(0.9999, length(yNames) + 1),
      breaks = 1:(length(yNames) + 1),
      labels = c(yNames, ""),
      minor_breaks = FALSE
    ) +
    theme(
      axis.text.x = element_text(
        hjust = 0,
        vjust = 1,
        colour = "grey50"
      ),
      axis.text.y = element_text(
        hjust = 0,
        vjust = 0,
        angle = 90,
        colour = "grey50"
      )
    )
  p
}
