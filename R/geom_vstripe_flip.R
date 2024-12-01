#' Vertical stripe with constant fill
#'
#' Displays a vertical stripe that spans the entire height of the plot for each pair of `x` and  `xend` values. Optional `alpha` defaults to `1`.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and `inherit.aes = TRUE` (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position A position adjustment to use on the data for this layer.
#' @param ... Other arguments passed on to `layer()`'s params argument.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes	If `FALSE`, overrides the default aesthetics, rather than combining with them.
#'
#' @export
#' @importFrom rlang %||%
#' @importFrom grid rectGrob gTree gList unit gpar
#' @importFrom scales alpha
#' @importFrom ggplot2 aes layer ggproto Geom
#'
#' @examples
#' stripes_df <- data.frame(x = c(1, 2, 3),
#'                          xend = c(2, 3, 4),
#'                          fill = c("lightgray", "darkgray", "lightgray"))
#' ggplot2::ggplot(stripes_df) +
#'   geom_vstripe_flip(ggplot2::aes(y = x, yend = xend), fill = stripes_df$fill)
geom_vstripe_flip <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", ...,
                              show.legend = NA, inherit.aes = FALSE) {

  GeomVerticalStripe <- ggproto(
    "GeomGradientStripe", Geom,

    required_aes = c("y", "yend", "fill"),

    default_aes = ggplot2::aes(alpha = 1),

    draw_panel = function(data, panel_params, coord) {
      # Transform data using the coordinate system
      coords <- coord$transform(data, panel_params)

      rect_grobs <- lapply(1:nrow(coords), function(i) {
        row <- coords[i, ]

        # Create a raster grob for the gradient fill
        if ("x" %in% colnames(coords)) {
          grid::rectGrob(
            x = unit(mean(c(row$x, row$xend)), "native"),
            y = unit(c(0.5, 0.5), "npc"),
            width = unit(row$xend - row$x, "native"),
            height = unit(1, "npc"),
            gp = gpar(fill = scales::alpha(row$fill, row$alpha), col = NA)
          )
        } else {
          grid::rectGrob(
            x = unit(mean(c(row$y, row$yend)), "native"),
            y = unit(c(0.5, 0.5), "npc"),
            width = unit(row$yend - row$y, "native"),
            height = unit(1, "npc"),
            gp = gpar(fill = scales::alpha(row$fill, row$alpha), col = NA)
          )
        }
      })

      gTree(children = do.call(gList, rect_grobs))
    }
  )

  layer(
    geom = GeomVerticalStripe,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}
