#' plot_wordcloud
#'
#' Make a custom wordcloud plot.
#' @param x Vector of sentences or words.
#' @param output Output path.
#' @param scale A vector of length 2 indicating the range of the size of the words.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap
#' file, if a positive integer. Also used for units other than the default.
#' If not specified, taken as 72 ppi to set the size of text and line widths.
#' @param fixed.asp if TRUE, the aspect ratio is fixed. Variable aspect ratio
#' only supported if rot.per==0
#' @param removePunctuation Remove punctuation. Default: TRUE.
#' @param removeNumbers Remove numbers. Default: TRUE.
#' @param stripWhitespace Remove whitespace. Default: TRUE.
#' @param colors Colors to use in wordcloud.
#' @keywords plot, wordcloud
#' @examples
#' # Create a vector of month names
#' x <- c()
#' for (i in 1:12) {
#'  x <- append(x, rep(month.name[i], i * 2))
#' }
#' plot_wordcloud(x, "./wordcloud.png")
#' @export
#' @import tm
#' @importFrom wordcloud wordcloud
#' @importFrom graphics par
#' @importFrom grDevices dev.off png
#' @importFrom dplyr pull


plot_wordcloud <- function(x, output = "./wordcloud.png", scale = c(4, .5),
                           width = 12, height = 7, units = "in", res = 150,
                           fixed.asp = FALSE, removePunctuation = TRUE,
                           removeNumbers = TRUE, stripWhitespace = TRUE,
                           colors = brand_cols()) {

  # Stem and remove stopwords for further analyses
  corpus <- Corpus(VectorSource(x))
  dtm <- TermDocumentMatrix(corpus, control = list(
    removePunctuation = removePunctuation,
    removeNumbers = removeNumbers,
    tolower = TRUE,
    stopwords = c(stopwords("finnish"), pull(fi_remove_words)),
    stripWhitespace = stripWhitespace)
    )

  vec <- sort(rowSums(as.matrix(dtm)), decreasing = TRUE)
  df <- data.frame(word = names(vec), freq = vec)

  # Save as a file
  png(output, width = width, height = height, units = units, res = res)
  par(mar = rep(0, 4))
  wordcloud(df$word, df$freq, scale = scale, min.freq = 1, max.words = 200,
            rot.per = 0, colors = colors, fixed.asp = fixed.asp)
  # Close file
  # Note: temp is needed to silent dev.off console output
  temp <- dev.off()
}
