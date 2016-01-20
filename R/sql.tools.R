require(berdie)
require(magrittr)
require(plyr)


#' reset.conn
#' @description Resets the last connection, handled by berdie::last_connection()
#' Honestly just a shortcut.
#' @export
reset.conn <- function(...) options()$avant.database.yml %>%
  berdie::postgresql_connection()


#' read.query
#' @description A wrapper for read_query, with some additional workarounds for
#' the way Windows handles text files.
#' @param x - query as a character string
#' @return Returns the modified character string with odd line-ending characters
#' removed, as well as any '--' style comments.
#' @export
read.query <- function(x) {
  x %>%
    readLines(encoding = "UTF-8") %>%
    gsub('--.*','',.) %>%
    paste(collapse=' ') %>%
    sub('\uFEFF','',.) #this last part gets rid of byte order marks.
}

#' run.query
#' @description is exactly berdie::read_query. I just like having function
#' names separated by .s and column names separated by _
#'
#' Edit: That's how this started. Then I realized that given how the average
#' query length is 20 mins longish and the DB has a tendency to reset itself
#' after a couple, there's no reason not to make conn default to a new
#' connection every time. Hopefully the DB is good at dropping connections.
#' Trololololol.
#'
#' Edit 2: So, broadly, there are two types of queries you might run. One is
#' standalone, you could put it into a pgAdmin window and be just fine. The other
#' is like row-lookup style - I need to hit the db for each row, and each
#' query I'm generating uses information from that row. This latter case, I've
#' noticed, runs into a weird error called "Inactive result set." I can't find
#' anyone online who's run into this, but it feels a bit like a locking error,
#' since it seems to happen to random entries. So I'm putting in some recursive
#' logic here to just rerun the query if this error hits. Also defaulting back
#' to last_connection() and using similar recursive logic to reset the connection
#' if it fails. So should be much more efficient about connection usage now.
#' @param x - query as a character vector
#' @param conn - as the connection object. See berdie::postgresql_connection
#' @export
run.query <- function(x, conn=last_connection()){
  df <- try(run_query(x, conn))

  #Setting the stage for error handling.
  if(class(df) == 'try-error'){
    if(df[1] == "Error : Inactive result set\n"){df <- run.query(x)}
    if(df[1] == "Error : is(conn, \"PqConnection\") is not TRUE\n"){
      reset.conn()
      run.query(x)
    }
  }
  # print(df)
  return(df)
}


#These are just helper functions for the functions below. Won't export.
query.split <- function(q) strsplit(q, " ")[[1]]
query.combine <- function(q) paste(q,collapse=" ")
date.reg <- "'\\d+[/-]\\d+[/-]\\d+'"


#' find.dates
#' @description Will parse query text and returns base on return.place. If true
#' function returns a vector of the available dates in a query. If false,
#' returns a vector of the logical position of dates in the vector created by
#' query.split.
#' @param q - query as a single character string
#' @param return.place - switch for showing date values, or date locations
#' @return If return.place = F, function returns a string of booleans, one
#' for each word in the query, where T indicates that the string is coded as
#' a date. If return.place = T, the function returns a vector of each date
#' string coded into the query.
#' @export
find.dates <- function(q, return.place = F){

  is.date.text <- function(x) grepl(date.reg, x)
  v <- sapply(query.split(q), is.date.text, USE.NAMES=F)
  if(return.place){return (v)} else{
    return(query.split(q)[v])
  }
}


#' replace.dates
#' @description Replaces query dates with a vector of dates specified by the
#' user. Function will error if length(dates) is not the same as dates found by
#' find.dates.
#' @param q - query as a single character string
#' @param dates - a vector of dates to replace in the query. Function will error
#' if length(dates) != length(find.dates(q,T))
#' @return Function returns the original query text, but with the new dates.
#' @export
replace.dates <- function(q, dates){
  #'If the date entries don't have quotes, add them.
  print(dates)
  dates[not(grepl("'",dates))] <- paste0("'",dates,"'")

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

#' file.paths
#' @description Takes an input file path and returns the full system path
#' for each file found at the input path.
#' @param x - a single path, expressed as a character string
#' @return Function returns a vector of the global paths for each file found
#' in x.
#' @export
file.paths <- function(x) paste(x,list.files(x),sep="/")

#' get.files
#' @description Takes an input file path, look for all .csv files, and then
#' append each file in that path into a single data frame.
#' @param x - a single path, expressed as a character string
#' @return Function returns a single dataframe, which is the appended result
#' of all .csv files at the path x.
#' @export
get.files <- function(x) {
  csvs <- grep('\\.csv$',file.paths(x),value=TRUE)
  do.call(rbind,lapply(csvs, read.csv))
}

#' These two will enumerate the .csv files in a path, as well as load them
#' into a data frame.
