add_drug_10cm <- function(data, main = "diag1", uid = "seq_no"){
  require(tidyverse, quietly = T)
  require(lazyeval, quietly = T)
  
  f1 <- function(x) ifelse(as.numeric(x) %in% c(1:4), x, NA)
  
  # function to select intent 1 to 4 from 6th character
  f_intent6 <- compose(f1, 
                       function(x){
    gsub("(?!^(?:T3[679]9|T414|T427|T4[3579]9))\\w{5}(\\w)\\w{1}", "\\1", x, perl=TRUE )
  }
  )
  
  # function to select intent 1 to 4 from 5th character
  f_intent5 <- compose(f1, 
                       function(x){
    gsub("(?=^(?:T3[679]9|T414|T427|T4[3579]9))\\w{4}(\\w)\\w{2}", "\\1", x, perl=TRUE )
  }
  )
  # function to capture the encounter type
  f_encounter <- function(x) substr(x, 7,7)
  
  # defining and subsetting the drug cases
  .drugs_10cm <- "^T3[6-9]|^T4|^T50"

  drugset <- data[c(uid, main)] %>% 
    mutate(drugs = create_diag(., expr = .drugs_10cm, colvec = 2))
    
  drugset <- drugset %>% filter(drugs == 1) 

  drugset  <- drugset  %>% 
    mutate_(intent5 = interp(~ f_intent5(x), x = as.name(main)), 
            intent6 = interp(~ f_intent6(x), x = as.name(main)),
            encounter = interp(~ f_encounter(x), x = as.name(main)))
  
  drugset  <- drugset  %>% mutate(intent = coalesce(intent6, intent5))
  drugset  <- filter(drugset , encounter %in% c("A", "D") & intent %in% c(1,2,3,4) )
  
  # add the drug subset to the original dataset
  drugset_j <- data %>% 
    left_join(select(drugset, -intent5, -intent6)) %>%
                       replace_na(list(drugs = 0)) 
  
  drugset_j
}

