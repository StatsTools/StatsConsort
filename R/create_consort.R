#' Create consort
#'
#' @param xsize A number with x-axis dimension.
#' @param ysize A number with y-axis dimension.
#'
#' @return A consort.
#' @export
#'
#' @examples
#' x <- create_consort(100, 150)
create_consort <- function(xsize, ysize) {

 # Validation Step -------------------------------------------------------------
 stopifnot(
  "`xsize` must be provided." = !is.na(xsize),
  "`xsize` must be numeric." = is.numeric(xsize),
  "`xsize` cannot be an array." = length(xsize) == 1,
  "`xsize` must be greater than one." = xsize > 1
 )

 stopifnot(
  "`ysize` must be provided." = !is.na(ysize),
  "`ysize` must be numeric." = is.numeric(ysize),
  "`ysize` cannot be an array." = length(ysize) == 1,
  "`ysize` must be greater than one." = ysize > 1
 )
 # -----------------------------------------------------------------------------

 consort <- new('Consort')

 return(create_consort_method(consort, xsize = xsize, ysize = ysize))
}
