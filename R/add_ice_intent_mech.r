#' Add ICECI intent and mechanism of injury
#'
#', reference
#' @param data input data
#' @param uid unique id variable without quotation marks
#' @param underlying_cause index of the underlying cause variable
#'
#' @return return the input with additional variables (ice_intent, ice_mechanism)
#'
#' @export
#' @importFrom fuzzyjoin regex_left_join
#'
#' @examples
#'

add_ice_intent_mech <- function (data, uid, underlying_cause, ignore_case = TRUE)
{
  uid <- enquo(uid)
  underlying_cause <- enquo(underlying_cause)
  underly <- "underly"
  ice_code <- ice_code10 %>%
    rename(ice_mechanism = ice_name,
           ice_intent = intent) %>%
    select(underly, ice_intent, ice_mechanism)
  suppressWarnings(suppressMessages(require(fuzzyjoin)))
  data %>% mutate(!!underly := !!underlying_cause) %>%
    fuzzyjoin::regex_left_join(ice_code, by = c(underly = "underly"), ignore_case = ignore_case) %>%
    filter(!duplicated(!!uid)) %>%
    rename(underly = underly.x) %>%
    select(-underly.y)


}
