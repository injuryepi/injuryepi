split_right <- function(x,pattern)
{
  #to keep right of the splitting pattern
  require(stringr, quietly = T)
  str_sub(x,str_locate(x,pattern)[,2]+1,-1)
}
