#' Add intent and mechanism combined fields for injury ICD-9-CM.
#'
#', reference
#' @param data input data
#' @param inj_col ecode and diagnosis column indices
#' @param reference injury ice reference with three possible choices:
#' "both"(the default) adding intent and mechanism combined variables
#' "intent" adding intent only
#' "mechanism" adding mechanism only
#'
#' @return return the input with additional variables (intent_mechanism combinations, intent variables, or mechanism variables depending on the reference choice
#'
#' @export
#' @importFrom purrr map2_dfc
#'
#' @examples
#' library(tidyverse)
#' dat <- data.frame(e1 = c("E9585", "E9885", "E986", "E966"),
#' e2 = c("E9050", "E9583", "E846", "E9556" ))
#'
#' dat %>% create_intent_mech(inj_col = c(1,2), reference = "both")
#' dat %>% create_intent_mech(inj_col = c(1,2), reference = "intent")
#' dat %>% create_intent_mech(inj_col = c(1,2), reference = "mechanism")
#'

add_intent_mech_icd9cm <- function(data, inj_col, reference = c("both", "intent", "mechanism")) {

  icd9cm_inj <- switch(match.arg(reference), both = icd9cm_intent_mech_regex, intent = icd9cm_intent_regex, mechanism = icd9cm_mech_regex)

  list_int_mech <- icd9cm_inj %>% pull(intent_mechanism)
  list_expr <- icd9cm_inj %>% pull(icd9cm_regex)

  f_im <- function(data = data, inj_col, var_name, expr) {
    var_name <- quo_name(var_name)

    data %>% mutate(!!var_name := add_new_diag(., expr = expr, colvec = inj_col)) %>% select(!!var_name)
  }
  dat2 <- map2_dfc(.x = list_int_mech, .y = list_expr, ~f_im(data = data, inj_col = inj_col, var_name = .x, expr =.y))

  data %>% bind_cols(dat2)
}
