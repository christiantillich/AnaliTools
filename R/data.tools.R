
#' Keep only named columns from df. 
keep <- function(df, c) colnames(df[colnames(df) %in% c])

#' Remove named columns from df
del <- function(df, c) colnames(df[!colnames(df) %in% c])

#' Gives me postgres-like coalesce function. 
coalesce <- function(..., null.func=is.na) {
  Reduce(function(x, y) {
    i <- which(null.func(x))
    x[i] <- y[i]
    x},
    list(...))
}

#DataSci's coalesce function. 
`%||%` <- function(x, y) if (is.null(x)) y else x 

#Create an empty data frame
empty.frame <- function(x) data.frame(row.names = x) %>% t



##### Compare Two Data Frames
data.frame.compare <- function(df,df2,by){
  
  #This may be a big operation, so clear unused memory
  gc()
  
  #Join the data frames together
  df.fin <- merge(df, df2,by = by,all=T)
  
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
  recs.1 <- df.fin[x] %>% sapply(is.na) %>% apply(1,all) %>% which
  if(length(recs.1) > 0){
    warning(paste("There are",length(recs.1),"unmatched records in table 1"))
    out$table.1.unmatched <- df.fin[recs.1, keys]
  }
  
  
  recs.2 <- df.fin[y] %>% sapply(is.na) %>% apply(1,all) %>% which
  if(length(recs.2) > 0){
    warning(paste("There are",length(recs.2),"unmatched records in table 2"))
    out$table.2.unmatched <- df.fin[recs.2, keys]
  }
  
  
  
  #Are there any differing values for any of the entries
  df.fin[is.na(df.fin)] <- 'NA'
  v <- -c(recs.1,recs.2)
  if(length(v) == 0){v <- rep(T, nrow(df.fin))}
  recs <- apply(as.data.frame(df.fin[v,x] != df.fin[v,y]), 1, any)
  if(sum(recs) > 0){
    warning(paste("There are",sum(recs),"mismatched records in the full set"))
  }
  out$mismatches <- recs[recs == T] %>% names %>% as.integer %>% df.fin[.,keys]
  
  
  return(out)
}