date_to_year <- function(date_start, date_end) {
	suppressWarnings(suppressMessages(require(lubridate)))
	date_start %--% date_end %/% years(1)
}
