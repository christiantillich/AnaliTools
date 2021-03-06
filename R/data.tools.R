
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



#' coalesce
#' @description SQL-like coalesce
#' @return SQL-like coalesce, e.g. coalesce(x,y)
#' @rdname coalesce
#' @export
#' @examples coalesce(x,y)
coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}



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
#' @param exclude -
#' @return A dataframe that summarizes the data, including total count, # nulls
#' # uniques, variable class, and text showing some examples.
#' @export
#' @examples var.sum(iris)
var.sum <- function(df, exclude='values'){

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
  # print(table(t$type))

  return(t[,del(t,exclude)])
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
  ,mar=c(0,0,3,0)
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
cap <- function(x, lbnd=-Inf, ubnd=Inf) min(max(x, lbnd),ubnd)

#' s.cap
#' @description An sapply wrapper for cap.
#' @param v - A vector input.
#' @param lbnd - The lower bound. Defaults to -Inf.
#' @param ubnd - The upper bound. Defaults to Inf
#' @return Returns the vector, where values are replaced by the bound if they
#' exceed that limit.
#' @export
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
l.cap <- function(v, lbnd=-Inf, ubnd=Inf){
  lapply(v,function(y) cap(y,lbnd,ubnd))
}



#' cat.cap
#' @description Takes a vector of categorical values and collapses the vector
#' into the first 'lim' unique values (by volume), and the rest are collapsed to
#' an 'other' category.
#' @param vector - The input vector
#' @param lim - A numeric limit on the number
#' @param ret - The category that all other values are collapsed into. Defaults
#' to 'other'.
#' @return Returns the vector, where all values not in the x top values are
#' replaced with the value for ret.
#' @export
cat.cap <- function(vector, lim=50, ret='other'){
  col.sum <- vector %>% table %>% sort(T) %>% head(lim)

  vector %>% table %>% sort(T) %>%
    data.frame(
      val=names(.)
      , count=.
      , pct=cumsum(.)/sum(.)
      ,row.names=NULL
    ) %>% head(lim) %>% textplot('left','top',mar=c(0,0,0,0), cex=0.75, cmar=1)

  ifelse(
    as.character(vector) %in% names(col.sum)
    ,as.character(vector)
    ,ret
  ) %>% return
}



#' impute.var
#' @description Creates a quick linear model to impute null values based on
#' values in the data. Will only replace nulls, the predicted model values are
#' discarded.
#' @param df The data frame.
#' @param var The name of the variable to impute null values for, as a string
#' @param formula The formula for the imputation model
#' @param plots A variable to turn diagnostic plots on or off.
#' @return Returns the original data frame with a new variable labeled Designed
#' to put into magrittry chains easily.
#' @export
impute.var <- function(df, var, formula, plots=F){

  #Name of the new variable
  new.var = paste0('imp_',var)

  #Create the model for imputation.
  mdl <- lm(paste(var,'~',formula),data = df)

  #Create the imputed vector, add it to the data frame
  #Doing it this way gets the na's right.
  y.hat <- predict(mdl, newdata=df)
  df[new.var] <- ifelse(!is.na(df[[var]]), df[[var]], y.hat)

  #If plots = true, plot a time series by month of the real variable
  #superimposed over the imputation. Also maybe return the model summary?
  if(plots){

    #Imputation model
    mdl %>% summary %>% coef %>% round(4) %>% data.frame.plot(halign="center")
    title(paste0("Imputation Model - ",var))

    #Shows the time-series with the raw and imputed values
    plot(
      df %>%
        mutate(funding.mth = as.Date(as.yearmon(Funding.Date))) %>%
        group_by(funding.mth) %>%
        summarise_(
          raw = interp(~mean(val, na.rm = TRUE), val = as.name(var))
          ,interp =
             interp(~mean(val,na.rm= TRUE),val=as.name(new.var))
          ,count = interp(~n())
        ) %>%
        qplot(funding.mth, raw, data=., alpha=count, geom=c("point","line")) +
        geom_line(aes(y=interp),linetype="dashed", alpha=I(1)) +
        labs(title="Interpolated Values", y = var) +
        theme_BA()
    )

    #Show the difference between the prediction (y.hat) and the real.
    v <- !is.na(df[var]) & !is.na(df[new.var])
    plot(
      perf.plot(df[v,var], df[v, new.var]) +
        geom_abline(slope=1, linetype = "dashed") +
        labs(title = paste0("Error Plot - ", var), y = "Actual", x ="Expected")
    )
  }

  return(df)
}

#' zero.out
#' @description Replaces NA/Inf values with 0.
#' @param x - A numeric vector.
#' @return Returns x but with NA/Inf values replaced with zero.
#' @export
zero.out <- function(x){
  x[is.na(x) | is.infinite(x)] <- 0
  return(x)
}




#' cum.sum
#' @description - A better cumulative summing function. This one ignores
#' NA/Inf values in the cumulative sum's history, but also returns NA for
#' the values that are explicitly NA/Inf. The result noticeable graphically -
#' Troublesome values are treated as missing, but the missing value itself
#' does not cause a problem when computing the prior history.
#' @param x - A numerical vector
#' @return - Returns a cumulative sum vector that is more suitable to graphing.
#' @export
cum.sum <- function(x) {
  ifelse(is.na(x) | is.infinite(x), as.numeric(NA), cumsum(zero.out(x)))
}


#' clip.tail
#' @param x - a vector whose tail you'll be clipping.
#' @param repl - The value to replace. Defaults to zero. #'
#' @return - Returns a vector where any continuous series of repl at the end of
#' the vector is replace with NA
#' @export
clip.tail <- function(x, repl=0){
  v <-  (x == 0) | is.na(x) | is.infinite(x)
  v <- as.logical(rev(cumprod(rev(v))))
  x[v] <- as.numeric(NA)
  return(x)
}


#' toss.tail
#'
#' @param x - a vector whose tail you'll be tossing.
#' @param n - the number of observations to replace with null.
#' @return - Returns a vector where the last n non-NA values are replaced with
#' NA.
#' @export
toss.tail <- function(x, n){
  pos <- max(which(!is.na(x)))
  vals <- head(x, pos-n)
  return(c(vals,rep(NA, length(x) - length(vals))))
}


#' file.paths
#' @description Takes an input file path and returns the full system path
#' for each file found at the input path.
#' @param x - a single path, expressed as a character string
#' @return Function returns a vector of the global paths for each file found
#' in x.
#' @export
file.paths <- function(x) paste(x,list.files(x),sep="/")


#' combine.data
#' @description Takes an input file path, look for all .csv files, and then
#' append each file in that path into a single data frame.
#' @param x - a path to a single directory, expressed as a character string
#' @param match - A user-input regex to match to a subset of the filenames in a
#' directory. Will only get files where grep(match)=T. The default value matches
#' any .csv
#' @return Function returns a single dataframe, which is the appended result
#' of all .csv files at the path x.
#' @export
combine.csvs <- function(x) {
  csvs <- grep('\\.csv$',file.paths(x),value=TRUE)
  do.call(rbind,lapply(csvs, read.csv))
}


#' corr.plot
#' @param df data.frame. A dataframe whose features you want to inspect the 
#' correlations of. 
#' @return A graph object from igraph::dataframe. Can be fed straight into plot.
#' The plot pushes more heavily correlated variables closer to each other and
#' less correlated variables further apart. Useful for evaluating potential
#' factor structures in large data sets. 
#' @export
corr.plot <- function(df){
  corrs <- df %>% 
    cor(use = "pairwise.complete.obs") %>%
    melt %>%
    filter(abs(value) > 0.40) %>% 
    mutate(
       weight = value^2
      ,type = factor(ifelse(value >= 0, 'pos','neg'))
    )
  
  net <- graph_from_data_frame(
      d = corrs
    , vertices = unique(corrs$Var1)
    , directed=FALSE
  ) %>% simplify(remove.loops = TRUE)
  
  V(net)$size <- 1
  net
}
