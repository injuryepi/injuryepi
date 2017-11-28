add_violent_types <- function(data,underly_col) {
    
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
    
nvdrs_suicide_regex_ <- "X[67]|X8[0-4]|Y870"
nvdrs_homicide_regex_ <- "X8[5-9]|X9|Y0|Y871"
nvdrs_legal_regex_ <- "Y35[0-467]|Y890"

nvdrs_undetermined_regex_ <- "Y[12]|Y3[0-4]|Y872|Y899"
nvdrs_unintent_firearm_regex_ <- "W3[2-4]" # consider Y86 when due to firearm
nvdrs_terror_regex_ <- "U0[123]"

    data %>% mutate(nvdrs_suicide = create_diag(., expr = nvdrs_suicide_regex_ , colvec = underly_col), 
                    nvdrs_homicide = create_diag(., expr = nvdrs_homicide_regex_ , colvec = underly_col), 
                    nvdrs_legal = create_diag(., expr = nvdrs_legal_regex_ , colvec = underly_col), 
                    nvdrs_undetermined = create_diag(., expr = nvdrs_undetermined_regex_ , colvec = underly_col),
                    nvdrs_unintent_firearm = create_diag(., expr = nvdrs_unintent_firearm_regex_ , colvec = underly_col),
                    nvdrs_terror = create_diag(., expr = nvdrs_terror_regex_ , colvec = underly_col)) %>% 
        mutate(violent_types = as.factor(ifelse(nvdrs_suicide == 1, 1, ifelse(nvdrs_homicide == 
            1, 2, ifelse(nvdrs_legal == 1, 3, ifelse(nvdrs_undetermined == 
            1, 4, ifelse(nvdrs_terror == 1,5, ifelse(nvdrs_unintent_firearm == 1, 6, NA)))))))) %>% 
    select(-nvdrs_suicide, -nvdrs_homicide, -nvdrs_legal, -nvdrs_undetermined, -nvdrs_unintent_firearm, -nvdrs_terror) %>%
    
    mutate(violent_labels = fct_recode(violent_types, 
                             "suicide" = "1", 
                            "homicide"  = "2",
                            "legal" = "3",
                            "undetermined" = "4",
                            "terrorism" = "5",
                             "accidental firearm" = "6"))
}
