# Waapihk graphics colours
waapihk_colors <- c(
  `sage`     = '#5d7b7c',
  `dust`     = '#c9a790',
  `soil`     = '#9d968d',
  `ice`      = '#e2eeef',
  `compost`  = '#7a7c78',

  `grey_1`   = '#a4b4bc',
  `grey_2`   = '#849ca4',
  `grey_3`   = '#7c9097'


)

#' Function to extract colors as hex codes
#'
#' @param ... Character names of waapihk_colors. Colour names include 'sage', 'dust',
#' 'soil', 'ice',  'compost', 'grey_1','grey_2', 'grey_3'.
#' @examples waapihk_cols()
#' @examples waapihk_cols('sage')
#'
#' @return A character vector of one or more colours in Waapihk's palette
#'
#' @export
waapihk_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (waapihk_colors)
  waapihk_colors[cols]
}

waapihk_palettes <- list(
  `main`      = waapihk_cols('sage', 'dust', 'soil', 'ice', 'compost'),
  `greys` = waapihk_cols('grey_1', 'grey_2', 'grey_3')
)

#' Return function to interpolate a Waapihk color palette
#'
#' @param palette Character name of palette in waapihk_palettes. Currently, one of 'main' or 'greys'.
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @examples waapihk_pal()(3)
#' @examples waapihk_pal(palette = 'greys')(3)
#'
#' @return A character vector of interpolated colour values
#'
#' @export
waapihk_pal <- function(palette = 'main', reverse = FALSE, ...) {
  pal <- waapihk_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal, ...)
}

#' Color scale constructor for colors
#'
#' @param palette Character name of palette in waapihk_palettes. Currently, one of 'main' or 'greys'.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @examples
#' library(ggplot2)
#' ggplot(diamonds, aes(depth, color = cut)) +
#'   geom_density() +
#'   xlim(55, 70) +
#'   scale_color_waapihk()
#'
#' @return A ggplot2 colour scale object, to be passed to a ggplot2 object
#'
#' @export
scale_color_waapihk <- function(palette = 'main', discrete = TRUE, reverse = FALSE, ...) {
  pal <- waapihk_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale('colour', paste0('waapihk_', palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname scale_color_waapihk
#' @export
scale_colour_waapihk <- scale_color_waapihk

#' Fill scale constructor for colors
#'
#' @param palette Character name of palette in waapihk_palettes. Currently, one of 'main' or 'greys'.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @examples
#' library(ggplot2)
#' ggplot(diamonds, aes(carat, fill = cut)) +
#'   geom_density(position = 'fill') +
#'   scale_fill_waapihk()
#'
#' @return A ggplot2 colour scale object, to be passed to a ggplot2 object
#'
#' @export
scale_fill_waapihk <- function(palette = 'main', discrete = TRUE, reverse = FALSE, ...) {
  pal <- waapihk_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale('fill', paste0('waapihk_', palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' ggplot Waapihk theme
#'
#' Theme function to be passed as an argument during ggplot2 charting.
#'
#' @param size Base font size
#' @param family Font family
#' @param title Font to use for title text
#' @param position How should headings be positioned? Options are 'center' or 'left'. (Default: 'center',)
#'
#' @return A ggplot2 theme object, to be passed to a ggplot2 object
#'
#' @export

theme_waapihk <- function(base_size = 18,
                          base_family = "Open Sans") {
  font <- base_family

  ggplot2::theme(
    plot.margin = unit(c(.5, .8, 0, .8), "cm"),
    plot.background = ggplot2::element_rect(fill = "#ffffff", colour = NA),
    plot.title = ggplot2::element_text(family = font, size = 24, lineheight = 1.2, face = "bold", color = "#222222", margin = ggplot2::margin(0, 0, 0, 0)),
    plot.subtitle = ggplot2::element_text(family = font, size = 16, lineheight = 1, margin = ggplot2::margin(5, 0, 10, 0)),
    # This leaves the caption text element empty, because it is set elsewhere in the finalize plot function
    plot.caption = ggplot2::element_blank(),



    # Legend format
    # This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.key.width = unit(3, "mm"),
    legend.key.height = unit(3, "mm"),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = 14, color = "#222222", lineheight = 1.2),



    # Axis format
    # This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(family = font, size = 12, color = "#000000"),
    axis.text = ggplot2::element_text(family = font, size = 12, color = "#222222"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_line(color = "#777777"),
    axis.line = ggplot2::element_line(color = "#777777"),
    # axis.line.x=ggplot2::element_blank(),
    # axis.line.y=ggplot2::element_blank(),

    # Grid lines
    panel.grid.major = ggplot2::element_blank(),
    # panel.grid.major.x = ggplot2::element_blank(),
    # panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.minor = ggplot2::element_blank(),
    # panel.grid.minor.x = ggplot2::element_blank(),
    # panel.grid.minor.y = ggplot2::element_line(color="#cbcbcb"),
    panel.spacing = unit(2, "lines"),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "#ffffff"),

    # Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill = "#ffffff"),
    strip.text = ggplot2::element_text(family = font, face = "bold", color = "#555555", size = 14, margin = ggplot2::margin(5, 0, 10, 0), hjust = 0)
  )
}

