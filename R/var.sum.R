#' This function takes a data frame as an input and spits out a summary data 
#' frame.That frame lists out the following features: total count, class, 
#' null count, unique count, and a character string of the first couple values


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