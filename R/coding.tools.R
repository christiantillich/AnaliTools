#Turns a vector into the string that would represent that vector. 
vec.text <- function(x) {
  x %>% paste(collapse="','") %>% paste("c('",.,"')",sep="")
}


