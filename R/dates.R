#' Convert ISO year-week to date
#'
#' Converts character string or vector in format "2020-W34" or "2020-24" to
#' thursday date in week
#'
#' @param date Converts character string or vector in format "2020-W34" or "2020-24"
#' @export
gfunk.yearweek2date <- function(date) {

  ISOweekday <- function(date) {
    date <- as.Date(date)
    return(as.integer((as.integer(format(date, "%w"))+6) %% 7 + 1))
  }

  weekday0 <- function(date) {
    return(ISOweekday(date) - 1L)
  }

  thursday0 <- function(date) {
    date <- as.Date(date)
    return(date - weekday0(date) + 3)
  }

  stopifnot(all(is.na(date) | stringr::str_detect(date, "\\d{4}-W?\\d{2}")))
  if (all(is.na(date))) {
    return(rep(as.Date(NA_character_), length.out = length(date)))
  }
  year <- as.integer(stringr::str_extract(date, "\\d{4}"))
  week <- as.integer(stringr::str_extract(date, "(?<=\\d{4}-W?)\\d{2}"))
  stopifnot(all(is.na(week) | (1 <= week & week <= 53)))

  january04 <- as.Date(ifelse(is.na(year), NA, paste(year,
                                                     "01", "04", sep = "-")))
  first_thursday <- thursday0(january04)
  nearest_thursday <- first_thursday + 7 * (week - 1)
  return(nearest_thursday)
}
