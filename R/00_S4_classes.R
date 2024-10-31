if (is.null(getClassDef("gg"))) {
  setOldClass("gg")
}

# S4 Class 'Box' ---------------------------------------------------------------

setClass(
 'Box',
 slots = c(
  id = 'character',
  x_center = 'numeric',
  x_hl = 'numeric',
  y_center = 'numeric',
  y_hl = 'numeric',
  align = 'character',
  text = 'character',
  fontsize = 'numeric'
 )
)

# S4 Class 'Arrow' -------------------------------------------------------------

setClass(
 'Arrow',
 slots = c(
  begin = 'Box',
  end = 'Box',
  direction = 'character'
 )
)

# S4 Class 'Sidebox' -----------------------------------------------------------

setClass(
 'Sidebox',
 contains = 'Box'
)

# S4 Class 'Consort' -----------------------------------------------------------

setClass(
 'Consort',
 slots = c(
  xsize = 'integer',
  ysize = 'integer',
  gg = 'gg',
  boxes = 'list',
  arrows = 'list',
  sideboxes = 'list'
 ),
 prototype = list(
  xsize = NA_integer_,
  ysize = NA_integer_,
  gg = NA,
  boxes = list(),
  arrows = list(),
  sideboxes = list()
 )
)

# 'Consort' methods ------------------------------------------------------------

setGeneric('create_consort_method', function(x, xsize, ysize) standardGeneric('create_consort_method'))
setMethod('create_consort_method', 'Consort', function(x, xsize, ysize) {
 data <- tibble::tibble(expand.grid(x = seq(1, xsize), y = seq(1, ysize))) |>
  dplyr::filter(x <= y)

 ggplot2::theme_set(ggplot2::theme_get() + ggplot2::theme(text = ggplot2::element_text(size = 8)))

 gg <- data |>
  ggplot2::ggplot(ggplot2::aes(x, y)) +
  ggplot2::scale_x_continuous(limits = c(0, xsize), breaks = seq(1, xsize, 1)) +
  ggplot2::scale_y_continuous(limits = c(0, ysize), breaks = seq(1, ysize, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = '', y = '') +
  ggplot2::theme(panel.grid.major = ggplot2::element_line(linetype = 'dashed', colour = "gray"))

 x@xsize <- as.integer(xsize)
 x@ysize <- as.integer(ysize)
 x@gg <- gg

 return(x)
})

setGeneric('add_box_to_gg_method', function(x, box) standardGeneric('add_box_to_gg_method'))
setMethod('add_box_to_gg_method', 'Consort', function(x, box) {
 gg <- x@gg

 if (box@align == 'l') {
  hjust <- 0
  x_pos <- box@x_center - box@x_hl + (x@xsize * 1 / 100)
 } else if (box@align == 'c') {
  hjust <- 0.5
  x_pos <- box@x_center
 } else if (box@align == 'r') {
  hjust <- 1
  x_pos <- box@x_center + box@x_hl - (x@xsize * 1 / 100)
 }

 gg_updated <- gg +
  ggplot2::geom_rect(xmin = box@x_center - box@x_hl, xmax = box@x_center + box@x_hl,
            ymin = box@y_center - box@y_hl, ymax = box@y_center + box@y_hl,
            color = 'black', fill = 'white', size = 0.25) +
  ggplot2::annotate('text', x = x_pos, y = box@y_center, label = box@text, size = box@fontsize, hjust = hjust)

 return(gg_updated)
})

setGeneric('add_box_method', function(x, id, x_center, x_hl, y_center, y_hl, align, text, fontsize) standardGeneric('add_box_method'))
setMethod('add_box_method', 'Consort', function(x, id, x_center, x_hl, y_center, y_hl, align, text, fontsize) {

 box <- new('Box', id = id, x_center = x_center, x_hl = x_hl, y_center = y_center, y_hl = y_hl, align = align, text = text, fontsize = fontsize)
 x@boxes <- append(x@boxes, box)

 x@gg <- add_box_to_gg_method(x, box)

 return(x)
})

setGeneric('add_arrow_to_gg_method', function(x, arrow) standardGeneric('add_arrow_to_gg_method'))
setMethod('add_arrow_to_gg_method', 'Consort', function(x, arrow) {
 gg <- x@gg

 if (arrow@direction == 'top') {
  if (arrow@begin@x_center == arrow@end@x_center) {
   gg_updated <- gg +
    ggplot2::geom_segment(x = arrow@begin@x_center, xend = arrow@end@x_center, y = arrow@begin@y_center - arrow@begin@y_hl, yend = arrow@end@y_center + arrow@end@y_hl + (x@xsize * 0.3 / 100), size = 0.5,
                 linejoin = "mitre", lineend = "butt", arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type = "closed"))
  } else {
   gg_updated <- gg +
    ggplot2::geom_segment(x = arrow@begin@x_center, xend = arrow@begin@x_center, y = arrow@begin@y_center - arrow@begin@y_hl, yend = ((arrow@begin@y_center - arrow@begin@y_hl) + (arrow@end@y_center + arrow@end@y_hl)) / 2, size = 0.5,
                 linejoin = "mitre", lineend = "butt") +
    ggplot2::geom_segment(x = arrow@begin@x_center, xend = arrow@end@x_center, y = ((arrow@begin@y_center - arrow@begin@y_hl) + (arrow@end@y_center + arrow@end@y_hl)) / 2, yend = ((arrow@begin@y_center - arrow@begin@y_hl) + (arrow@end@y_center + arrow@end@y_hl)) / 2, size = 0.5,
                 linejoin = "mitre", lineend = "butt") +
    ggplot2::geom_segment(x = arrow@end@x_center, xend = arrow@end@x_center, y = ((arrow@begin@y_center - arrow@begin@y_hl) + (arrow@end@y_center + arrow@end@y_hl)) / 2, yend = arrow@end@y_center + arrow@end@y_hl + (x@xsize * 0.3 / 100), size = 0.5,
                 linejoin = "mitre", lineend = "butt", arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type = "closed"))
  }
 } else if (arrow@direction == 'side') {
   if (arrow@begin@x_center < arrow@end@x_center) {
     gg_updated <- gg +
      ggplot2::geom_segment(x = arrow@begin@x_center, xend = arrow@begin@x_center, y = arrow@begin@y_center - arrow@begin@y_hl, yend = arrow@end@y_center, size = 0.5,
                   linejoin = "mitre", lineend = "butt") +
      ggplot2::geom_segment(x = arrow@begin@x_center, xend = arrow@end@x_center - arrow@end@x_hl - (x@xsize * 0.2 / 100), y = arrow@end@y_center, yend = arrow@end@y_center, size = 0.5,
                    linejoin = "mitre", lineend = "butt", arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type = "closed"))
   } else if (arrow@begin@x_center > arrow@end@x_center) {
     gg_updated <- gg +
       ggplot2::geom_segment(x = arrow@begin@x_center, xend = arrow@begin@x_center, y = arrow@begin@y_center - arrow@begin@y_hl, yend = arrow@end@y_center, size = 0.5,
                             linejoin = "mitre", lineend = "butt") +
       ggplot2::geom_segment(x = arrow@begin@x_center, xend = arrow@end@x_center + arrow@end@x_hl + (x@xsize * 0.2 / 100), y = arrow@end@y_center, yend = arrow@end@y_center, size = 0.5,
                             linejoin = "mitre", lineend = "butt", arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type = "closed"))
   }
 }

 return(gg_updated)
})

setGeneric('add_arrow_method', function(x, begin_id, end_id, direction) standardGeneric('add_arrow_method'))
setMethod('add_arrow_method', 'Consort', function(x, begin_id, end_id, direction) {

 ids <- sapply(x@boxes, function(x) return(x@id))
 boxes_ids <- tibble::tibble(ID = seq(1, length(x@boxes)), id_name = ids)

 begin <- boxes_ids |> dplyr::filter(id_name == begin_id) |> dplyr::pull(ID)
 end <- boxes_ids |> dplyr::filter(id_name == end_id) |> dplyr::pull(ID)

 arrow <- new('Arrow', begin = x@boxes[[begin]], end = x@boxes[[end]], direction = direction)

 if (arrow@direction == 'side' & (arrow@begin@x_center == arrow@end@x_center)) {
   stop('End arrow "x" must be different than Begin arrow "x".')
 }

 x@arrows <- append(x@arrows, arrow)
 x@gg <- add_arrow_to_gg_method(x, arrow)

 return(x)
})

setGeneric('add_sidebox_to_gg_method', function(x, sidebox) standardGeneric('add_sidebox_to_gg_method'))
setMethod('add_sidebox_to_gg_method', 'Consort', function(x, sidebox) {
 gg <- x@gg

 if (sidebox@align == 'l') {
  hjust <- 0
  x_pos <- sidebox@x_center - sidebox@x_hl + 1
 } else if (sidebox@align == 'c') {
  hjust <- 0.5
  x_pos <- sidebox@x_center
 } else if (sidebox@align == 'r') {
  hjust <- 1
  x_pos <- sidebox@x_center + sidebox@x_hl - 1
 }

 gg_updated <- gg +
  statebins:::geom_rrect(xmin = sidebox@x_center - sidebox@x_hl, xmax = sidebox@x_center + sidebox@x_hl,
            ymin = sidebox@y_center - sidebox@y_hl, ymax = sidebox@y_center + sidebox@y_hl,
            color = '#4F81BD', fill = '#A9C7FD', size = 0.25) +
  ggplot2::annotate('text', x = x_pos, y = sidebox@y_center, label = sidebox@text, size = sidebox@fontsize,
           hjust = hjust, fontface = 2, colour = '#4F81BD')

 return(gg_updated)
})

setGeneric('add_sidebox_method', function(x, id, x_center, x_hl, y_center, y_hl, align, text, fontsize) standardGeneric('add_sidebox_method'))
setMethod('add_sidebox_method', 'Consort', function(x, id, x_center, x_hl, y_center, y_hl, align, text, fontsize) {

 sidebox <- new('Sidebox', id = id, x_center = x_center, x_hl = x_hl, y_center = y_center, y_hl = y_hl, align = align, text = text, fontsize = fontsize)
 x@sideboxes <- append(x@sideboxes, sidebox)

 x@gg <- add_sidebox_to_gg_method(x, sidebox)

 return(x)
})

setGeneric('clean_consort_method', function(x) standardGeneric('clean_consort_method'))
setMethod('clean_consort_method', 'Consort', function(x) {

 x@gg <- x@gg +
  ggplot2::theme_void() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"))

 return(x)
})

setGeneric('display_consort_method', function(x, fdim) standardGeneric('display_consort_method'))
setMethod('display_consort_method', 'Consort', function(x, fdim) {

 x <- clean_consort_method(x)

 temp_jpeg <- tempfile(fileext = ".jpeg")
 ggplot2::ggsave(filename = temp_jpeg, plot = x@gg, width = fdim$width, height = fdim$height, dpi = fdim$dpi, units = "in", device = "jpeg", quality = 100)
 img <- magick::image_read(temp_jpeg)
 magick::image_browse(img)

 return(x)
})

setGeneric('get_consort_method', function(x) standardGeneric('get_consort_method'))
setMethod('get_consort_method', 'Consort', function(x) return(x@gg))
