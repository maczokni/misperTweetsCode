#function for counting occurences of a char in string
#countchars
#' @export
countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}
