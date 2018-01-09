add_count_var_regex <- function(data, expr, var_name, ...) {
  # expr = regular expressions for the data
  # var_name = name for the new variable  
  # ... = columns of interest (columns with the diagnoses indices or names without quotes)
  require(dplyr, quietly = T)
  require(tidyr, quietly = T)
  require(purrr, quietly = T)
  
  # name to assign to the new variable
  var_name <- enquo(var_name)
  var_name <- quo_name(var_name)
  # select the variables of interest
  sel <- quos(...)
  
  #a function to assign '1' if the regular expression matched 
  f1 <- function(x) as.numeric(grepl(expr, x, ignore.case = ignore.case, perl = perl))
  f2 <- function(x) as.numeric(rowSums(x, na.rm = TRUE) } 

  df <- data %>% select(!!!sel) %>% 
    mutate_all(funs(as.character)) %>% 
    map_df(f1) %>% 
    mutate(new_diag = f2(.)) %>% 
    pull(new_diag)
  # the vector of the new variable counting the ones to add to the data 
  data %>% add_column(!!var_name := df)
  
}
