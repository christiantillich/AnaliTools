#' vec.text
#' @description Turns a vector into the string that would represent that vector.
#' For when you need to skip the bullshit and hardcode a list into the code.
#' @param x - any character vector
#' @return A text string formatted like an R vector, ie "c(var1, var2, ...)"
#' @export
vec.text <- function(x) {
  x %>% 
    format(scientific = F) %>% 
    trimws() %>%
    paste(collapse="','") %>% 
    paste("c('",.,"')",sep="")
}


#' vec.chunker
#' @description Splits a vector into a list where each list element contains
#' the next n values from the vector.
#' @param x - any vector
#' @return Returns a list
#' @export
vec.chunker <- function(x, n=100) split(x, ceiling(seq_along(x)/n))



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


#' is.date.text
#' @param x - An input string
#' @return Function returns logical, to indicate whether the string could be
#' turned into a date.
#' @export
#' @examples
is.date.text <- function(x){
  if(class(try(as.Date(x),T)) == "Date" && length(x) > 0) {return(T)} else {return(F)}
}



#' find.dates
#' @description Will parse query text and returns base on return.place. If true
#' function returns a vector of the available dates in a query. If false,
#' returns a vector of the logical position of dates in the vector created by
#' query.split.
#' @param text - query as a single character string
#' @param return.place - switch for showing date values, or date locations
#' @param date.reg - The regular expression for the date string. Allows the
#' user to change to match different formatting, if necessary.
#' @return If return.place = F, function returns a string of booleans, one
#' for each word in the query, where T indicates that the string is coded as
#' a date. If return.place = T, the function returns a vector of each date
#' string coded into the query.
#' @export
find.dates <- function(
   text
  ,return.place = F
  ,date.reg = "\\d+[/-]\\d+[/-]\\d+"
  ,text.split = function(x) x %>% as.character %>% strsplit(" ") %>% unlist
){

  is.date <- function(x) grep(date.reg, x,value=T) %>% is.date.text
  v <- sapply(text.split(text), is.date, USE.NAMES=F)
  if(return.place){return (v)} else{
    return(text.split(text)[v])
  }
}




#' replace.dates
#' @description Replaces query dates with a vector of dates specified by the
#' user. Function will error if length(dates) is not the same as dates found by
#' find.dates.
#' @param text - query as a single character string
#' @param dates - a vector of dates to replace in the query. Function will error
#' if length(dates) != length(find.dates(text,T))
#' @return Function returns the original query text, but with the new dates.
#' @export
replace.dates <- function(
   text
  ,dates
  ,date.reg = "\\d+[/-]\\d+[/-]\\d+"
  ,text.split = function(x) x %>% as.character %>% strsplit(" ") %>% unlist
  ,text.combine = function(x) paste(x,collapse=" ")
){

  #If the date entries don't have quotes, add them.
  # dates[not(grepl("'",dates))] <- paste0("'",dates,"'")

  #Get the split text and the location of dates in the text.
  v <- find.dates(text, T,date.reg,text.split)
  words <- text.split(text)

  #Logical check. Stop if input date lengths don't match with original text.
  if(length(words[v]) != length(dates)) {
    stop("Input Dates and Query Dates aren't same length")
  }

  #Do the replacement.
  replace.date.text <- function(x,y) gsub(date.reg, y, x)
  words[v] <- mapply(replace.date.text,words[v],dates,USE.NAMES = F)
  return(text.combine(words))
}
