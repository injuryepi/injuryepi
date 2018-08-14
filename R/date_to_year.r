#' Calculate number of years between two dates
#'
#' @param date_start: start date
#' @param date_end: end date

#' @return number of years
#' @export
#'
#' @examples to be added

#'
date_to_year <- function(date_start, date_end) {
	suppressWarnings(suppressMessages(require(lubridate)))
	date_start %--% date_end %/% years(1)
}
