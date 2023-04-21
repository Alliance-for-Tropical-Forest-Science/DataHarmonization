#' GenerateComment
#'
#' @description Helper function to concatenate comments, separated by ";"
#' @param x a character vector
#' @param comment The string to add in the column (character)
#'
#' @return concatenation of x and comment, separated by a semicolon
#'
#' @export
#' @examples
#' GenerateComment(letters, "A")

GenerateComment <- function(x, comment){

  pattern = "(.*)(?=;\\1)"
  gsub("^;|;;+", "", gsub(pattern,  "", paste(x, comment, sep = ";"), perl = T))


}

