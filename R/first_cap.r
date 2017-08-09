
first_cap <- function(x){
suppressWarnings(suppressMessages(require(purrr)))
compose(function(x) gsub("(^|[[:space:]])([[:alpha:]])", "\\\\1\\\\U\\\\2", x, perl=TRUE), function(x) tolower(x))
}
