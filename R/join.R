#' Join dataframe with dictionary
#'
#' Joins dictionary with dataframe and renames dictionary variable. Dictionary is a tibble or
#' dataframe with two columns: one with values matching the specified column in the supplied
#' dataframe, and one with the "new" or translated values". After joining, the column with the
#' translated values will be renamed to the name of the old column with a suffix.
#'
#' @param .data A dataframe/tibble
#' @param dict A dataframe or tibble with 2 columns
#' @param by a one length named character vector specifying which columns to join by
#' @param suffix a character to append to new column name
#' @return A joined dataframe
#' @export
join_dictionary <- function(.data, dict, by = NULL, suffix = '.dict') {
  stopifnot(is.character(by), length(by) == 1,
            is_tibble(dict) | is.data.frame(dict), length(dict) == 2,
            is.character(suffix))

  new_name <- paste0(names(by)[1], suffix)
  old_name <- names(dict)[names(dict) != by[1]]

  .data %>% left_join(dict, by = by) %>%
    rename(!! sym(new_name) := sym(old_name))

}
