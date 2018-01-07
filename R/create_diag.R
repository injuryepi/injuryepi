create_diag <- function(data, expr, colvec, ignore.case = T, perl = T){
  #expr = regular expressions 
  # colvec = vector of the columns of interest (columns with the diagnoses)
  require(dplyr, quietly = T)
  require(tidyr, quietly = T)
  require(purrr, quietly = T)
  # assign '1' if the regular expression matched 
  f1 = function(x) as.numeric(grepl(expr, x, ignore.case = ignore.case, perl = perl))
  # any one in the diagnosis field suffices
  f2 = function(x){as.numeric(rowSums(x, na.rm = TRUE) > 0)} 
 
  data %>% select(colvec) %>% 
    mutate_all(funs(as.character)) %>% 
    map_df(f1) %>% 
    mutate(new_diag = f2(.)) %>% 
    pull(new_diag)
  
}
