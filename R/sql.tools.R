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
#' @param match - A user-input regex to match to a subset of the filenames in a
#' directory. Will only get files where grep(match)=T. The default value matches
#' any .csv
#' @return Function returns a single dataframe, which is the appended result
#' of all .csv files at the path x.
#' @export
get.files <- function(x,match='\\.csv$') {
  csvs <- grep(match,file.paths(x),value=TRUE)
  do.call(rbind,lapply(csvs, read.csv))
}



#' query.chunker
#' @description This function chunks a query into smaller portions, to improve
#' runtime performance and get around that 2 hour forced limit on the DB. The
#' query must only contain two text strings formatted as a date; if there are
#' additional hardcoded dates, this function will break.
#' @param query.path - The path to the query, as a string.
#' @param data.path - The path to dump the data into, as a string
#' @param start.date - The start date for the query.
#' @param end.date - The end date for the query.
#' @param interval - A string input for the interval to chunk by. See 'by' input
#' parameter from seq(). Defaults to '1 month'.
#' @return NULL
#' @export query.chunker
query.chunker <- function (
   query.path
  ,data.path
  ,start.date
  ,end.date
  ,interval='1 month'
){
  #Get the name of the query we're executing.
  file.name <- query.path %>% gsub('.sql','',.,fixed=T) %>% gsub('.+/','',.)

  #Force inputs to dates.
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)

  #Make sure the user knows what's going on.
  print(paste('Pulling files between', start.date, 'and', end.date))

  #Grab the core query.
  query <- read.query(query.path)


  #These are the months we'll be running for
  dates <- seq(start.date, end.date, by = interval) %>%
    as.character

  #Run that shit.
  for(i in 1:(length(dates)-1)){
    reset.conn()
    d <- replace.dates(query, c(dates[i], dates[i+1])) %>%  run.query()
    write.csv(d,paste0(data.path,'/',file.name,"_",gsub('[/-]','_',dates[i]),".csv"))
  }

  print("Data Retrieved Successfully!")
}

# setwd('C:/Users/ctilli82/Desktop/query_batcher_test')
# query.batcher('queries/test_1.sql','Data','2015-01-01','2015-01-10',interval='1 day')
# query.batcher('queries/test_2.sql','Data','2015-05-01','2015-05-03',interval='1 day')
# query.batcher('queries/test_3.sql','Data','2015-09-01','2015-09-04',interval='1 day')


