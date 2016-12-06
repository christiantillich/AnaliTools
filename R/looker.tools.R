#' looker.chunker
#' @description This function takes a looker query and chops it up into smaller
#' portions, ideally to improve runtime. The looker dictionary must only have
#' two date fields, and they must be sequentially ordered so start.date comes
#' first.
#' @param looker.path - The path to the file that contains the looker dictionary
#' @param data.path - The path to your data dump
#' @param start.date - The start date of the query
#' @param end.date - The end date of the query.
#' @param interval - String input for the interval size. See the 'by' input
#' parameter from seq()
#' @return Returns null. The function writes out the data frames to data.path.
#' @export
looker.chunker <- function(
   looker.path
  ,data.path
  ,start.date
  ,end.date
  ,interval='1 month'
){
  #Make sure that start and end dates are dates
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)

  #Get the name of the looker query we're executing.
  file.name <- looker.path %>% gsub('.R','',.,fixed=T) %>% gsub('.+/','',.)

  #Retrieve the looker query.
  source(looker.path)

  #Create the sequence of dates to chunk by.
  dates <- seq(start.date, end.date, by = interval) %>%
    as.character %>%
    gsub('-','/',.,fixed=T)

  #A function that replaces dates if they're found
  f <- function(x,d){
    if(length(find.dates(x)) > 0){
      replace.dates(x,d)
    }else{x}
  }

  #For every element in our date vector...
  for (i in 1:(length(dates) - 1)) {

    print(paste("Running looker between",dates[i],"and",dates[i+1]))

    #Break apart the query lines, replace the dates, then execute.
    (function(y){
      y %>% sapply(function(x) f(x,c(dates[i],dates[i+1])), USE.NAMES = F)
    }) %>%
      lapply(dictionary, .) %>%
      do.call(looker_query, .) %>%
      write.csv(paste0(data.path, "/", file.name, "_", gsub("[/-]","_", dates[i]), ".csv"))
  }
}




#' clean_looker_colnames
#' @description Transforms looker's standard formatting into a much more
#' data-friendly snake case format. NOTE: The column names must already be
#' formatted according to the URL formatting used to call the query. Looker3's
#' pretty formatting will fail. To work around this, replace the column names
#' with the vector you specified in the dictionary used to call the query.
#' @param df The dataframe being altered.
#' @return Returns the data frame with the new snake case column names.
#' @export
clean_looker_colnames <- function (df){

  colnames(df)      <- gsub("^[^.]+\\.", "", colnames(df))
  is_id_col         <- grepl('_id', colnames(df))
  df[is_id_col]     <- lapply(df[is_id_col], function(x) as.integer(as.character(x)))
  is_date_col       <- grepl('_date$', colnames(df))
  df[is_date_col]   <- lapply(df[is_date_col], function(x) {as.Date(as.character(x), '%Y-%m-%d')})
  # character_col     <- vapply(df, is.character, logical(1))
  # df[character_col] <- lapply(df[character_col], as.factor)
  df
}
