onclip <- function (x, row.names = F, col.names = T)
{
  write.table(x, file = "clipboard", sep = "\t", row.names = row.names, col.names = col.names)
}