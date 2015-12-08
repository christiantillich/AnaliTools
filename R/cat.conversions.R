

as.flgs <- function(df,x){
  #' This function converts a single column into a matrix of binary
  #' flags, one for each unique value the column can take. The columns
  #' can then be readily joined with the original data frame using cbind.
  #' Be careful with this function at present, as you can easily flood 
  #' memory by choosing columns with large numbers of unique values.   
  
  #If any of the columns are not already factors, make them factors
  df <- sapply(df, as.factor)
  
  #Convert the chosen factor column into a matrix of binary flags.
  M <- as.data.frame(model.matrix(~df[,x] - 1))
  names(M) <- gsub('df\\[, x\\]',paste(x,"_",sep=""), names(M))
  
  #Pick the column with the greatest sum. Drop it. This is your reference.
  N <- names(colSums(M)[order(colSums(M), decreasing=T)[1]])
  M[,N] <- NULL
  
  return(M)
}

flg.create <- function(df, name.list){
  #' This function is a wrapper for as.flgs, to apply it to a named list
  #' of columns in a target data frame. This allows you to do batch the
  #' flag conversion of factor variables. As above, be careful with this
  #' function, as it's easy to flood memory by batching more than one
  #' column with large numbers of unique entries. 
  
  #Apply as.flgs to every column name in name.list
  flgs <- lapply(name.list, function(x) as.flgs(df,x))
  
  #flgs returns as a list. Unlist it and force to data frame with names. 
  d <- as.data.frame(unlist(flgs, recursive=F))
  names(d) <- gsub('.+\\.',"", names(d))
  
  #In the original data set, del every column in the named list. 
  df <- df[del(df, name.list)]
  
  #Append the new logical columns to the original data set. 
  df <- cbind(df, d)
  
  #Return the modified data set. 
  return(df)
}