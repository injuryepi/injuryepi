
first_cap <- compose(function(x) gsub("(^|[[:space:]])([[:alpha:]])", "\\\\1\\\\U\\\\2", x, perl=TRUE), function(x) tolower(x))

