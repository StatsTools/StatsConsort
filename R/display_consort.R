#' Display consort
#'
#' @param consort  An object with class Consort.
#' @param fdim A list of 3 elements 'width', 'height' and 'dpi', respectively, with 3 numeric values associated. Only used for type = 'F'.
#'
#' @return A consort.
#' @export
#'
#' @examples
#' x <- create_consort(100, 150) |>
#'  add_box('box1', x = c(10, 5), y = c(50, 3), align = 'c', text = 'Box 1') |>
#'  display_consort()
display_consort <- function(consort, fdim = list(width = 9, height = 5, dpi = 600)) {

 # Validation Step -------------------------------------------------------------
 stopifnot("`consort`must be a Consort object." = class(consort) == 'Consort')

 stopifnot(
  "`fdim` must be provided." = !is.na(fdim),
  "`fdim` must be a list." = is.list(fdim),
  "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = length(fdim) == 3,
  "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = names(fdim) == c('width', 'height', 'dpi'),
  "`fdim` values must be numeric." = is.numeric(unlist(fdim))
 )
 # -----------------------------------------------------------------------------

 return(display_consort_method(x = consort, fdim = fdim))
}
