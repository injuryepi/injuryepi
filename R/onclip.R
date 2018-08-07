#' Copy to clipboard. A wrapper to write.table
#'
#', reference
#' @param x input data
#' @param row.names default to F
#' @param col.names  default to Tl
#'
#' @return table saved to clipboard
#'
#' @export
#' @importFrom
#'
#' @examples
#'

onclip <- function (x, row.names = F, col.names = T)
{
  write.table(x, file = "clipboard", sep = "\t", row.names = row.names, col.names = col.names)
}
