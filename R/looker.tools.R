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


#' Convert a Looker url to a well-formed query for the API.
#'
#' @description - This is a straight-up rip-off of avant::looker_url_to_query. I'm
#' adding it here because of issues I have with installing avant locally. Will
#' remove as soon as local avant installs no longer become a problem.
#' @param url character. The full URL copy pasted from the Looker view.
#' @return a list of arguments to be passed to LookerQuery.
#' @note a limit of 99999999 will always be added to the output since we almost always want all the data.
#' @author RobK
#' @examples /dontrun{
#' stopifnot(identical(
#'   looker_url_to_query("https://avantcredit.looker.com/explore/onyx/default_data?fields=default_data.loan_id,default_data.installment_id,default_data.installment_number,default_data.already_defaulted&f%5Bdefault_data.first_30_defaulted_installment%5D=Yes&limit=500&title=all+30+day+default+installments&look_id=63&show=data,fields&vis=%7B%22type%22:%22looker_column%22%7D&sorts=default_data.loan_id&filter_config=%7B%22default_data.first_30_defaulted_installment%22:%5B%7B%22type%22:%22is%22,%22values%22:%5B%7B%22constant%22:%22Yes%22%7D,%7B%7D%5D,%22id%22:0%7D%5D%7D"),
#'   list("onyx", "default_data",
#'      c("default_data.loan_id", "default_data.installment_number", "default_data.first_defaulted_installment"),
#'      c("default_data.installment_number: 0 to 3"), limit = 99999999
#'   )
#' )}
#' @export
looker_url_to_query <- function(url) {
  stopifnot(is.character(url))
  parsed_url <- httr::parse_url(url)

  # parsed_url$path looks like "explore/onyx/default_data"
  path <- strsplit(parsed_url$path, "/")[[1]]
  dictionary <- path[2]
  look <- path[3]

  query <- parsed_url$query

  if (is.null(query$fields)) {
    stop("No fields found. Did you 'Expand the URL' before copying it into R? \n",
         "Try press the gear at the top-right and Share (or Cmd+U)")
  }

  fields <- strsplit(query$fields, ",")[[1]]

  filters <- query[grepl('f[', names(query), fixed = TRUE)]
  names(filters) <- gsub("^f\\[|\\]$", "", names(filters))
  filters[] <- lapply(filters, chartr, old = '+', new = ' ')
  filters <- paste(names(filters), unlist(filters), sep = ': ')

  list(dictionary, look, fields, filters, limit = 99999999)
}
