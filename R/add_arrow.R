#' Add arrow to consort
#'
#' @param consort An object with class Consort.
#' @param begin_id A character with the id of the box to start the arrow.
#' @param end_id A character with the id of the box to end the arrow.
#' @param direction A character with the direction of the arrow. Use 'top' or 'side'.
#'
#' @return A consort.
#' @export
#'
#' @examples
#' x <- create_consort(100, 150) |>
#'  add_box('box1', x = c(10, 5), y = c(50, 3), align = 'c', text = 'Box 1') |>
#'  add_box('box2', x = c(10, 5), y = c(30, 3), align = 'c', text = 'Box 2') |>
#'  add_arrow('box1', 'box2', 'top')
add_arrow <- function(consort, begin_id, end_id, direction) {

 # Validation Step -------------------------------------------------------------
 stopifnot("`consort`must be a Consort object." = class(consort) == 'Consort')

 stopifnot(
  "`begin_id` must be provided." = !is.na(begin_id),
  "`begin_id` must be a character." = is.character(begin_id),
  "`begin_id` cannot be an array." = length(begin_id) == 1
 )

 stopifnot(
  "`end_id` must be provided." = !is.na(end_id),
  "`end_id` must be a character." = is.character(end_id),
  "`end_id` cannot be an array." = length(end_id) == 1
 )

 stopifnot(
  "`direction` must be provided." = !is.na(direction),
  "`direction` must be a character." = is.character(direction),
  "`direction` cannot be an array." = length(direction) == 1,
  "`direction` must be 'top' or 'side'." = direction %in% c('top', 'side')
 )

 boxes_ids <- sapply(consort@boxes, function(box) box@id)

 stopifnot("`begin_id` box doesn't exist." = begin_id %in% boxes_ids)
 stopifnot("`end_id` box doesn't exist." = end_id %in% boxes_ids)
 # -----------------------------------------------------------------------------

 return(add_arrow_method(x = consort, begin_id = begin_id, end_id = end_id, direction = direction))
}
