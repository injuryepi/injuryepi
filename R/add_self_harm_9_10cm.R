
#'
#' @param data the hospital dataset in which to add the new variables
#' @param diag_ecode_col The column indeces for the icd 9 and icd 10 cm
#' @param var_name The name for the new variable
#' @param date The name of the date of discharge field
#'
#' @return The original dataset with the added field. 1 means variable present and 0 otherwise
#' @export
#'
#' @examples Examples will be added later
#'
add_self_harm_9_10cm <- function(data, diag_ecode_col, var_name, date) {
  date <- enquo(date)
  expr9 <- "E95[0-9]"
  expr10 <- "^X7[1-9]|^X8[0-3]|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..2|((T3[679]9|T414|T427|T4[3579]9)2)|(?!(T5[1-467]9|T58[019]|T599|T609|T61[019]|T6[235]9|T64[08]))(T5[1-9]|T6[0-5])..2|((T5[1-467]9|T58[019]|T599|T609|T61[019]|T6[235]9|T64[08])2+)|T71..2|T1491"
  var_name <- enquo(var_name)
  var_name <- quo_name(var_name)


  data1 <- data %>%
    filter(od_fed_fiscal_year(!! date) < 2016) %>%
    mutate(!! var_name := od_create_diag(., expr = expr9, colvec = diag_ecode_col))


  data2 <- data %>% filter(od_fed_fiscal_year(!! date) > 2015) %>% mutate(!! var_name := od_create_diag(., expr = expr10, colvec = diag_ecode_col))

  data <- dplyr::bind_rows(data1, data2)

  data
}
