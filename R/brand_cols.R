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
#' @importFrom grDevices colorRampPalette

brand_pal <- function(palette = "main", reverse = FALSE, ...) {

  # Brand palettes
  brand_palettes <- list(
    `main`  = brand_cols("blue", "yellow", "red"),
    `main2`  = brand_cols("purple", "blue", "yellow", "red"),
    `cool`  = brand_cols("dark blue", "blue", "light blue"),
    `hot`   = brand_cols("red", "semi red", "light red"),
    `likert3` = brand_cols("blue", "blue", "yellow", "red", "red"),
    `likert5` = brand_cols("blue", "light blue", "yellow", "semi red", "red"),
    `likert5+` = brand_cols("blue", "light blue", "yellow", "semi red", "red", "grey"),
    `all`  = brand_cols("dark blue", "blue", "red", "yellow", "green", "purple", "grey")
  )


  pal <- brand_palettes[[palette]]

  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

#' Color scale constructor for Brand colors
#'
#' @param palette Character name of palette in drsimonj_palettes
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
