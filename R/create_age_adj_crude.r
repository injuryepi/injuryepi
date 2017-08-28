create_age_adj_crude <- purrr::compose(add_crude_inc_rate, calc_direct_age_adjust)
