#' Function like colorRamp, but handles better situations, where color vector is
#' larger than is required.
#'
#' @param colors TBD
#' @param n TBD
#' @param bias TBD
#' @param space TBD
#' @param interpolate TBD
#' @param alpha TBD
#' @importFrom stats approxfun splinefun
#' @export

colorRampD <- function (
  colors, n, bias = 1, space = c("rgb", "Lab"),
  interpolate = c("linear", "spline"), alpha = FALSE) {

  # PRELIMINARY STEPS ----------------
  if (bias <= 0)
    stop("'bias' must be positive")
  if (!missing(space) && alpha)
    stop("'alpha' must be false if 'space' is specified")
  colors <- t(col2rgb(colors, alpha = alpha)/255)
  space <- match.arg(space)
  interpolate <- match.arg(interpolate)

  # CUT THE COLOR VECTOR ----------------------

  if (space == "Lab")
    colors <- convertColor(colors, from = "sRGB", to = "Lab")
  interpolate <- switch(
    interpolate, linear = stats::approxfun,
    spline = stats::splinefun)

  # RESPECT ORDER IF NCLASSES < NCOLORS
  if (n<nrow(colors)) colors <- colors[1:n,]

  if ((nc <- nrow(colors)) == 1L) {
    colors <- colors[c(1L, 1L), ]
    nc <- 2L
  }
  x <- seq.int(0, 1, length.out = nc)^bias
  palette <- c(
    interpolate(x, colors[, 1L]),
    interpolate(x, colors[, 2L]),
    interpolate(x, colors[, 3L]),
    if (alpha) interpolate(x, colors[, 4L]))
  roundcolor <- function(rgb) pmax(pmin(rgb, 1), 0)
  if (space == "Lab")
    function(x) roundcolor(
      convertColor(
        cbind(
          palette[[1L]](x),
          palette[[2L]](x),
          palette[[3L]](x),
          if (alpha) palette[[4L]](x)),
        from = "Lab", to = "sRGB")) * 255
  else function(x) roundcolor(
    cbind(
      palette[[1L]](x),
      palette[[2L]](x),
      palette[[3L]](x),
      if (alpha) palette[[4L]](x))) * 255
}

#' Function like colorRampPalette to ramp palette unless number of colors vector
#' is larger than is required.
#' @param colors TBD
#' @param ... TBD
#' @export

colorRampPaletteD <- function (colors, ...){
  # n: number of classes
  function(n) {
    ramp <- colorRampD(colors, n, ...)
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
  }
}

#' Function to extract Brand colors as hex codes
#'
#' @param ... Character names of brand_colors
#' @export

brand_cols <- function(...) {

  # Brand colors
  brand_colors <- c(
    `dark blue` = "#202A44",
    `pink` = "#FFC0CB",
    `light blue` = "#B5D3E7",
    `yellow` = "#FFDA00",
    `orange` = "#FFA500",
    `blue` = "#1261A0",
    `grey` = "#BDBDBD",
    `red` = "#D6001C",
    `green` = "#2B5329",
    `light green` = "#90EE90",
    `light red` = "#FF7F7F",
    `semi red` = "#FF6961",
    `purple` = "#800080")

  cols <- c(...)

  if (is.null(cols))
    return (brand_colors)

  brand_colors[cols]
}

#' Return function to interpolate Brand color palette
#'
#' @param palette Character name of palette in brand_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export

brand_pal <- function(palette = "main", reverse = FALSE, ...) {

  # Brand palettes
  brand_palettes <- list(
    `main`  = brand_cols("blue", "yellow", "red"),
    `main2`  = brand_cols("purple", "blue", "yellow", "red"),
    `main_comparison`  = brand_cols("dark blue", "red", "yellow"),
    `cool`  = brand_cols("dark blue", "blue", "light blue"),
    `hot`   = brand_cols("red", "semi red", "light red"),
    `likert3` = brand_cols("blue", "blue", "yellow", "red", "red"),
    `likert5` = brand_cols("blue", "light blue", "yellow", "semi red", "red"),
    `likert5+` = brand_cols("blue", "light blue", "yellow", "semi red", "red", "grey"),
    `all`  = brand_cols("dark blue", "blue", "red", "yellow", "green", "purple", "grey")
  )


  pal <- brand_palettes[[palette]]

  if (reverse) pal <- rev(pal)
  colorRampPaletteD(pal, ...)
}

#' Color scale constructor for Brand colors
#'
#' @param palette Character name of palette in brand_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export

scale_color_brand <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- brand_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("brand_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for Brand colors
#'
#' @param palette Character name of palette in brand_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export

scale_fill_brand <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- brand_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("brand_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
