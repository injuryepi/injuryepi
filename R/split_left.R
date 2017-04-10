split_left <- function(x,pattern)
{
  #to keep left of the splitting pattern
  require(stringr, quietly = T)
  str_sub(x,,str_locate(x,pattern)[,1]-1)
  # split_left(x=c("1005.00","15.5"),pattern="\\.")
}

