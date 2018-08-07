ice_code9cm_intent_regex <- ice_code9cm %>%
  group_by(intent) %>%
  summarise(icd9cm_regex = make_regex(ecode1))

icd9cm_intent_regex <- ice_code9cm_intent_regex %>%
  mutate(intent = clean_intent_names(intent),
         intent_mechanism = intent)

devtools::use_data(icd9cm_intent_regex , compress = "xz", overwrite = T)
