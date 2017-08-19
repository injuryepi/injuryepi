injury_icd10cm <- "(?!(^T78|^T8[0-8]))(^S|^T)" 
# diagnosis code beginning with S or T, except T78 and T80-T88
# (?!) is  a negative lookhead . It looks the pattern to its right (^S|^T) and excludes (^T78|^T8[0-8])
