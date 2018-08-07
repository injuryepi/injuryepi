

ice_code9cm <- injuryepi::ice_code9 %>%
  select(ecode1, ice_name, intent) %>%
  rename(mechanism = ice_name) %>%
  mutate(mechanism = gsub("Fall/Jump/Push", "Fall", mechanism),
         mechanism = gsub("Drowning/Boating-Related|Drowning/Other", "Drowning", mechanism))

ice_code9cm_regex <- ice_code9cm %>%
  group_by(intent, mechanism) %>%
  summarise(icd9cm_regex = make_regex(ecode1))


icd9cm_intent_mech_regex <- ice_code9cm_regex %>%
  mutate(mechanism = clean_mech_names(mechanism))

icd9cm_intent_mech_regex <- icd9cm_intent_mech_regex %>%
  ungroup() %>%
  mutate(intent = clean_intent_names(intent),
         intent_mechanism = paste(intent, mechanism, sep = "_"))

devtools::use_data(icd9cm_intent_mech_regex , compress = "xz", overwrite = T)
