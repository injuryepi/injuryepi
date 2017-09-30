add_full_date <- function(x){
	# add 15/ between month and year for truncated date
	suppressWarnings(suppressMessages(require(lubridate)))

	lubridate::mdy(gsub( "(?<=^.{3})", "15/", x, perl=TRUE ))
}
