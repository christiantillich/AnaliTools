require(berdie)
require(magrittr)
require(plyr)


#Resets the last connection, handled by berdie::last_connection()
reset.conn <- function(...) options()$avant.database.yml %>%
  berdie::postgresql_connection()


read.query <- function(x) {
  #' #A wrapper for read_query, with some additional workarounds for 
  #' the way Windows handles text files. 
  x %>% 
    readLines(encoding = "UTF-8") %>% 
    paste(collapse=' ') %>%
    sub('\uFEFF','',.) #this last part gets rid of byte order marks. 
  
  #' TODO: This function collapses queries in a way that's harmful to
  #' comments made via --. 
}

#Purely for consistency. 
run.query <- run_query

query.split <- function(q) strsplit(q, " ")[[1]]
query.combine <- function(q) paste(q,collapse=" ")
date.reg <- "'\\d+[/-]\\d+[/-]\\d+'"

find.dates <- function(q, return.place = F){
  #' Will parse query text and returns base on return.place. If true
  #' function returns a vector of the available dates in a query. If 
  #' false, returns a vector of the logical position of dates in the
  #' vector created by query.split. 

  is.date.text <- function(x) grepl(date.reg, x)
  v <- sapply(query.split(q), is.date.text, USE.NAMES=F)
  if(return.place){return (v)} else{
    return(query.split(q)[v])
  }
}

replace.dates <- function(q, dates){
  #' Replaces query dates with a vector of dates specified by the user.
  #' Function will error if length(dates) is not the same as dates found
  #' by find.dates
  
  #Get the split query and the location of dates in the query. 
  v <- find.dates(q, T)
  text <- query.split(q)
  
  #Logical check. Stop query if lengths aren't the same. 
  if(length(text[v]) != length(dates)) {
    stop("Input Dates and Query Dates aren't same length")
  }
  
  #Do the replacement. 
  replace.date.text <- function(x,y) gsub(date.reg, y, x)
  text[v] <- mapply(replace.date.text,text[v],dates,USE.NAMES = F)
  return(query.combine(text))
}

#' These two will enumerate the .csv files in a path, as well as load them
#' into a data frame. 
file.paths <- function(x) paste(x,list.files(x),sep="/")
get.files <- function(x) do.call(rbind,lapply(file.paths(x), read.csv))