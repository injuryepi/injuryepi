add_ice_intent_mech <- function(data, underlying_cause){

	underlying_cause <- enquo(underlying_cause)
	underly <- "underly"


	ice_code <- ice_code10 %>%
		rename(ice_mechanism = ice_name, ice_intent = intent) %>%
		select(underly, ice_intent, ice_mechanism)

	suppressWarnings(suppressMessages(require(fuzzyjoin)))

	data %>%
		mutate(!!underly := !!underlying_cause) %>%
		fuzzyjoin::regex_left_join(ice_code, by = "underly")

}
