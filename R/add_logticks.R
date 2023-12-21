#' Add logticks
#'
#' @param base Base
#' @param sides Sides
#' @param scaled Scaled
#' @param short Short
#' @param mid Mid
#' @param long Long
#' @param colour Colour
#' @param size Size
#' @param linetype Linetype
#' @param alpha Alpha
#' @param color Color
#' @param data Data
#' @param ... Other
#'
#' @return Added logticks
#' @export

add_logticks  <- function (base = 10, sides = "bl", scaled = TRUE,
                           short = unit(0.1, "cm"), mid = unit(0.2, "cm"),  long = unit(0.3, "cm"),
                           colour = "black",  size = 0.5, linetype = 1, alpha = 1, color = NULL,
                           data =data.frame(x = NA),... )   {
  if (!is.null(color))
    colour <- color
  layer(geom = "logticks", params = list(base = base,
                                         sides = sides, scaled = scaled, short = short,
                                         mid = mid, long = long, colour = colour, size = size,
                                         linetype = linetype, alpha = alpha, ...),
        stat = "identity", data = data , mapping = NULL, inherit.aes = FALSE, position = "identity",
        show.legend = FALSE)
}
