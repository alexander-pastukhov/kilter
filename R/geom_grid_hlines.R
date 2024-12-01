#' Replicate horizontal grid lines
#'
#' Replicate horizontal grid lines that are hidden by stripes.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and `inherit.aes = TRUE` (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position A position adjustment to use on the data for this layer.
#' @param ... Other arguments passed on to `layer()`'s params argument.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes	If `FALSE`, overrides the default aesthetics, rather than combining with them.
#' @param color Line color, if `NULL` theme panel grid line color is used.
#'
#' @export
#' @importFrom rlang %||%
#' @importFrom grid linesGrob gTree gList unit gpar
#' @importFrom scales alpha
#' @importFrom ggplot2 aes layer ggproto Geom
#'
#' @examples
#' stripes_df <- data.frame(x = c(1, 2, 3), xend = c(2, 3, 4))
#' ggplot2::ggplot(stripes_df) +
#'   geom_gradient_vstripe(ggplot2::aes(x = x, xend = xend)) +
#'   geom_grid_hlines()
geom_grid_hlines <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", ...,
                             show.legend = NA, inherit.aes = TRUE, color = NULL) {

  GeomGridHLines <- ggproto(
    "GeomGridHLines", Geom,

    required_aes = character(0),

    draw_panel = function(data, panel_params, coord) {
      theme_current <- ggplot2::theme_get()

      line_y <- c()
      line_color <- c()
      line_lwd <- c()
      line_lty <- c()


      if (!inherits(theme_current$panel.grid.major.y, "element_blank")){
        breaks_n <- length(panel_params[['y']]$breaks)
        line_y <- c(line_y, panel_params[['y']]$breaks)
        line_color <- c(line_color, rep(color %||% theme_current$panel.grid.major$colour, breaks_n))
        line_lwd <- c(line_lwd, rep(theme_current$panel.grid.major$size %||% 0.5, breaks_n))
        line_lty <- c(line_lty, rep(theme_current$panel.grid.major$linetype %||% "solid", breaks_n))
      }

      if (!inherits(theme_current$panel.grid.minor.y, "element_blank")){
        breaks_n <- length(panel_params$y$minor_breaks)
        line_y <- c(line_y, panel_params$y$minor_breaks)
        line_color <- c(line_color, rep(color %||% theme_current$panel.grid.minor$colour, breaks_n))
        line_lwd <- c(line_lwd, rep(theme_current$panel.grid.minor$size %||% 0.5, breaks_n))
        line_lty <- c(line_lty, rep(theme_current$panel.grid.minor$linetype %||% "solid", breaks_n))
      }

      if (length(line_y) > 0) {
        # Transform horizontal grid positions to npc coordinates
        line_y_npc <- coord$transform(data.frame(y = line_y), panel_params)$y

        # replicate all lines
        grid_grobs <- lapply(1:length(line_y_npc), function(i) {
          linesGrob(
            x = unit(c(0, 1), "npc"),  # Full horizontal line
            y = unit(c(line_y_npc[i], line_y_npc[i]), "npc"),
            gp = gpar(col = line_color[i], lwd = line_lwd[i], lty = line_lty[i])  # Adjust line appearance
          )
        })

        gTree(children = do.call(gList, grid_grobs))
      }
    }
  )


  layer(
    geom = GeomGridHLines,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}
