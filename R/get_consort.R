#' Get consort plot
#'
#' @param consort  An object with class Consort.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' x <- create_consort(100, 150) |>
#'  add_box('box1', x = c(10, 5), y = c(50, 3), align = 'c', text = 'Box 1') |>
#'  display_consort() |>
#'  get_consort()
get_consort <- function(consort) {

 # Validation Step -------------------------------------------------------------
 stopifnot("`consort`must be a Consort object." = class(consort) == 'Consort')
 # -----------------------------------------------------------------------------

 return(get_consort_method(x = consort))
}
