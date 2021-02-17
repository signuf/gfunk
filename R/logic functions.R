#' Return NA of same Class as input
#'
#' @param x An object
#' @return NA of same class as object
#' @export
NA_class <- function(x) {
  # Returns NA value of same class as x
  switch(class(x),
         'Date' = as.Date(NA),
         'character' = as.character(NA),
         'logical' = as.logical(NA),
         'POSIXct' = as.POSIXct(NA),
         'difftime' = as.difftime(NA),
         'double' = as.double(NA),
         'factor' = as.factor(NA),
         'integer' = as.integer(NA),
         'POSIXlt' = as.POSIXlt(NA),
         stop("Class not recognized"))
}

#' Return 'true' or NA
#'
#' Evaluates 'condition' and returns 'true' if TRUE and NA if FALSE
#'
#' @param condition A condition, evaluating to a logical value
#' @param true value to return if true
#' @return true or NA of same class as true
#' @export
if_else_NA <- function(condition, true) if_else(condition, true, NA_class(true))

#' @export
'%!in%' <- function(x,y) !('%in%'(x,y))
