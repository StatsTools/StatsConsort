#' Add sidebox to consort
#'
#' @param consort An object with class Consort.
#' @param id A character with de the id of the sidebox. Must be unique.
#' @param x An array with 2 elements: first element is the position in x-axis and second is the half-length of the box in x-axis.
#' @param y An array with 2 elements: first element is the position in y-axis and second is the half-length of the box in y-axis.
#' @param align A character with the alignment of the text. Use 'l' to left, 'c' to center and 'r' to right.
#' @param text A character with the text inside the box.
#' @param fontsize A numeric with the font size of text inside the box.
#'
#' @return A consort.
#' @export
#'
#' @examples
#' x <- create_consort(100, 150) |>
#'  add_sidebox('box1', x = c(10, 5), y = c(50, 3), align = 'c', text = 'Box 1')
add_sidebox <- function(consort, id, x, y, align, text, fontsize = 8) {

 # Validation Step -------------------------------------------------------------
 stopifnot("`consort`must be a Consort object." = class(consort) == 'Consort')

 stopifnot(
  "`id` must be provided." = !is.na(id),
  "`id` must be a character." = is.character(id),
  "`id` cannot be an array." = length(id) == 1
 )

 stopifnot(
  "`x` must be provided." = !is.na(x),
  "`x` must be numeric." = is.numeric(x),
  "`x` must be an array with 2 elements." = length(x) == 2,
  "`x[1]` must be greater than one." = x[1] > 0,
  "`x[2]` must be greater than zero." = x[2] > 0
 )

 stopifnot(
  "`y` must be provided." = !is.na(y),
  "`y` must be numeric." = is.numeric(y),
  "`y` must be an array with 2 elements." = length(y) == 2,
  "`y[1]` must be greater than one." = y[1] > 0,
  "`y[2]` must be greater than zero." = y[2] > 0
 )

 stopifnot(
  "`align` must be provided." = !is.na(align),
  "`align` must be a character." = is.character(align),
  "`align` cannot be an array." = length(align) == 1,
  "`align` must be 'l', 'c' or 'r'." = align %in% c('l', 'c', 'r')
 )

 stopifnot(
  "`text` must be provided." = !is.na(text),
  "`text` must be a character." = is.character(text),
  "`text` cannot be an array." = length(text) == 1
 )

 stopifnot(
   "`fontsize` must be provided." = !is.na(fontsize),
   "`fontsize` must be numeric." = is.numeric(fontsize),
   "`fontsize` cannot be an array." = length(fontsize) == 1
 )

 stopifnot("`id` must be unique." = !(id %in% sapply(consort@sideboxes, function(box) box@id)))
 # -----------------------------------------------------------------------------

 return(add_sidebox_method(x = consort, id = id, x_center = x[1], x_hl = x[2], y_center = y[1], y_hl = y[2], align = align, text = text, fontsize = fontsize))
}
