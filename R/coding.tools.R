#' vec.text
#' @description Turns a vector into the string that would represent that vector.
#' For when you need to skip the bullshit and hardcode a list into the code.
#' @param x - any character vector
#' @return A text string formatted like an R vector, ie "c(var1, var2, ...)"
#' @export
vec.text <- function(x) {
  x %>% paste(collapse="','") %>% paste("c('",.,"')",sep="")
}


