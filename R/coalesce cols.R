#' Coalesce columns as characters
#'
#' Takes any number of vectors as input, coerces to character vectors and
#' uses dplyr::coalesce to coalesce them
#'
#' @param ... Vectors
#' @return A character vector
#' @export
coalesce_char <- function(...) {
  dplyr::coalesce(!!! lapply(list(...), as.character))
}

#' Coalesce Date columns
#'
#' Takes any number of vectors as input, coerces to character vectors and
#' uses dplyr::coalesce to coalesce them, then converts charactor vector
#' to Date vector
#'
#' @param ... Vectors
#' @return A date vector
#' @export
coalesce_date <- function(...) {
  coalesce_as_char(...) %>% as.Date()
}

#' Combine levels
#'
#' Takes any number of vectors as input, finds all unique levels of all
#' input vectos
#'
#' @param ... Vectors
#' @return A character vector
#' @export
combine_levels <- function(...) {
  unique(unlist(lapply(list(...), levels)))
}

#' Coalesce factor columns
#'
#' Takes any number of factor vectors as input, coerces them into charactor vectors.
#' Coerces charactor vector into factor with combined levels of input vectors.
#'
#' @param ... Vectors
#' @return A factor vector
#' @export
coalesce_factor <- function(...) {
  factor(coalesce_as_char(...), levels = combine_levels(...))
}
