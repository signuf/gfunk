#' Relevels categorical variable to factor
#'
#' Create factor from categorical variable with values stored as eg. numbers
#' (1, 4, 5, 99, etc) to new (actual) levels (man, woman, unknown). Input the numerical
#' or character vector and list of matched old value = new value pairs.
#'
#' @param x A categorical (numerical or character) variable
#' @param levels_list List of matched value pairs eg list('1' = 'mand', '2' = 'kvinde', etc)
#' @return Factor with specified levels
#' @export
factor_relevel <- function(x, levels_list) {
  x <- factor(x, levels = names(levels_list))
  levels(x) <- unlist(levels_list, use.names = F)

  return(x)
}
