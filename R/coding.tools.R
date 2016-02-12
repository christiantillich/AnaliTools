#' vec.text
#' @description Turns a vector into the string that would represent that vector.
#' For when you need to skip the bullshit and hardcode a list into the code.
#' @param x - any character vector
#' @return A text string formatted like an R vector, ie "c(var1, var2, ...)"
#' @export
vec.text <- function(x) {
  x %>% paste(collapse="','") %>% paste("c('",.,"')",sep="")
}



#' source.all
#' @description Why source one file when you can source a whole directory?
#' @param path - Path to the directory you wish to source
#' @return returns null
#' @export
source.all <- function(path){
  path %>%
    paste(., list.files(.), sep="/") %>%
    grep('.+\\.R',., value=T) %>%
    sapply(source)
}

#' snake.case
#' @param text - User Input Text
#' @return Function returns text that is formatted to snake case. 
#' @export
#' @examples
snake.case <- function(text){
  gsub('\\W+', '_', tolower(text))
}