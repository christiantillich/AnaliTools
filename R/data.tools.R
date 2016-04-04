
#' keep
#' @description - Keep only named columns from df. SAS-like keep function.
#' @param df - A data frame
#' @param c - A vector of column names.
#' @return Basically df %>% select()
#' @export
#' @examples keep(iris, 'Sepal.Length')
keep <- function(df, c) colnames(df[colnames(df) %in% c])



#' del
#' @description Remove named columns from df. SAS-like delete function.
#' @param df - A data frame
#' @param c - A vector of column names.
#' @return A data frame with the named columns removed.
#' @export
#' @examples keep(iris, 'Sepal.Length')
del <- function(df, c) colnames(df[!colnames(df) %in% c])

# #' Gives me postgres-like coalesce function.
# coalesce <- function(..., null.func=is.na) {
#   Reduce(function(x, y) {
#     i <- which(null.func(x))
#     x[i] <- y[i]
#     x},
#     list(...))
# }



#' %||%
#' @name %||%
#' @description DataSci's coalesce function. Gives postgres-like coalesce
#' @return equivalent to postgres coalesce(x,y)
#' @rdname coalesce
#' @export
#' @examples NULL %||% 'a'
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x



#' empty.frame
#' @description Create an empty data frame. Surprisingly tricky to do.
#' @param col.names - User input column names.
#' @return An empty data frame object with the specified column names
#' @export
empty.frame <- function(col.names) data.frame(row.names = col.names) %>% t



#' data.frame.compare
#' @description Compares two data frames by joining. Returns a summary containing
#' duplicate, missing, and differing records.
#' @param df - The first data frame
#' @param df2 - The second data frame
#' @param by - Used in merge(). The list of column names to join by. The default
#' on merge is all same-named columns, which would defeat a lot of the
#' functionality here. So the user must define the join explicitly.
#' @return A list object containing
#' @export
data.frame.compare <- function(df,df2,...){

  #This may be a big operation, so clear unused memory
  gc()

  #Join the data frames together
  df.fin <- merge(df, df2,...,all=T)

  #Define the joined columns, the .x columns, and the .y columns
  x <- colnames(df.fin) %>% grep('.x$',.) %>% colnames(df.fin)[.]
  y <- colnames(df.fin) %>% grep('.y$',.) %>% colnames(df.fin)[.]
  keys <- del(df.fin, c(x,y))

  #Create an empty list to add elements to
  out <- list()


  #Did any records get double-joined
  do.call(function(...) group_by_(df.fin,...), as.list(keys)) %>%
    summarise(
      count = n()
    ) %>%
    filter(count > 2) %>%
    as.data.frame -> recs
  # nrow(.) -> warn

  if(nrow(recs) > 0){
    warning(paste("One of the data sets has",sum(recs$count) - nrow(recs),"duplicate entries"))
  }
  out$duplicates <- recs



  #Did any record have a .x or .y that's all null?
  recs.1 <- df.fin[y] %>% sapply(is.na) %>% apply(1,all) %>% which
  if(length(recs.1) > 0){
    warning(paste("There are",length(recs.1),"records in table 1 that don't have a match in table 2"))
    out$table.1.unmatched <- df.fin[recs.1, c(keys,x)]
  }


  recs.2 <- df.fin[x] %>% sapply(is.na) %>% apply(1,all) %>% which
  if(length(recs.2) > 0){
    warning(paste("There are",length(recs.2),"records in table 2 that don't have a match in table 1"))
    out$table.2.unmatched <- df.fin[recs.2, c(keys,y)] %>% as.data.frame
  }






  #Are there any differing values for any of the entries
  df.fin[is.na(df.fin)] <- 'NA'
  v <- -c(recs.1,recs.2)
  if(length(v) == 0){v <- rep(T, nrow(df.fin))}
  # recs <- apply(as.data.frame(df.fin[v,x] != df.fin[v,y]), 1, any)
  recs <- (as.character(df.fin[v,x]) != as.character(df.fin[v,y])) %>%
    as.data.frame %>%
    apply(1,any)
  if(sum(recs) > 0){
    warning(paste("There are",sum(recs),"mismatched records in the full set"))
  }
  out$mismatches <- recs[recs == T] %>% names %>% as.integer %>% df.fin[.,keys]


  return(out)
}

#' var.sum
#'
#' @description This function takes a data frame as an input and spits out a summary data
#' frame.That frame lists out the following features: total count, class,
#' null count, unique count, and a character string of the first couple values
#' @param df - A dataframe
#' @return A dataframe that summarizes the data, including total count, # nulls
#' # uniques, variable class, and text showing some examples.
#' @export
#' @examples var.sum(iris)
var.sum <- function(df){

  #Give total count, null count, class, and unique count
  N <- sapply(df, length, USE.NAMES=F)
  n <- sapply(df, function(x) sum(is.na(x)),USE.NAMES=F)
  c <- sapply(df, class,USE.NAMES=F)
  u <- sapply(df, function(x) length(unique(x)),USE.NAMES=F)

  #Create unique list of values for each column
  f <- function(x){
    n <- as.character(head(unique(df[,x]),20))
    n[is.na(n)] <- "NA"
    n[n == ""] <- "<Empt>"
    g <- function(x) strtrim(paste(x, collapse=", "),40)
    if (nchar(g(n)) > 37){n <- paste(g(n),"...",sep="")}
    else(n <- g(n))
    return(n)
  }
  v <- sapply(names(N), f,USE.NAMES=F)


  #Establish the base data frame.
  t <- data.frame(cbind(names(N),N,c,n,u,v), row.names=NULL)
  colnames(t) <- c("var","N", "type", "nulls", "distinct", "values")
  t$N <- as.integer(as.character(t$N))
  t$nulls <- as.integer(as.character(t$nulls))
  t$distinct <- as.integer(as.character(t$distinct))
  print(table(t$type))
  return(t)
}



#' data.frame.plot
#' @description Just a wrapper for gplots::textplot, but with actually useful
#' starting parameters. For parameters beyond df, see textplot.
#' @param df - The data frame to plot
#' @return Returns NULL, but prints an image object displaying the data frame.
#' @export
data.frame.plot <- function(
   df
  ,halign='left'
  ,valign='top'
  ,mar=c(0,0,0,0)
  ,cex=0.75
  ,cmar=1
  ,...
){
  textplot(df,halign,valign,mar=mar, cex=cex, cmar=cmar,...)
}


#' cap
#' @description Imposes lower and upper bounds on a value.
#' @param x - Input
#' @param lbnd - The lower bound. Defaults to -Inf
#' @param ubnd - The upper bound. Defaults to Inf
#' @return Returns the input value if it's within the bounds. If the input
#' exceeds one of the bounds, cap returns the bound value instead.
#' @export
#' @examples
cap <- function(x, lbnd=-Inf, ubnd=Inf) min(max(x, lbnd),ubnd)

#' s.cap
#' @description An sapply wrapper for cap.
#' @param v - A vector input.
#' @param lbnd - The lower bound. Defaults to -Inf.
#' @param ubnd - The upper bound. Defaults to Inf
#' @return Returns the vector, where values are replaced by the bound if they
#' exceed that limit.
#' @export
#' @examples
s.cap <- function(v, lbnd=-Inf, ubnd=Inf){
  sapply(v,function(y) cap(y,lbnd,ubnd))
}

#' l.cap
#' @description An lapply wrapper for cap.
#' @param v - A list input.
#' @param lbnd - The lower bound. Defaults to -Inf.
#' @param ubnd - The upper bound. Defaults to Inf
#' @return Returns the list, where values are replaced by the bound if they
#' exceed that limit.
#' @export
#' @examples
l.cap <- function(v, lbnd=-Inf, ubnd=Inf){
  lapply(v,function(y) cap(y,lbnd,ubnd))
}
