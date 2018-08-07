
ice_code9cm_mech_regex <- ice_code9cm %>%
  group_by(mechanism) %>%
  summarise(icd9cm_regex = make_regex(ecode1))

icd9cm_mech_regex <- ice_code9cm_mech_regex %>%
  mutate(mechanism = clean_mech_names(mechanism),
         intent_mechanism = mechanism)

devtools::use_data(icd9cm_mech_regex, compress = "xz", overwrite = T)
