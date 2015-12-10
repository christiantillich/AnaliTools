#' as.flgs
#'
#' @description This function converts a single column into a matrix of binary
#' flags, one for each unique value the column can take. The columns can then
#' be readily joined with the original data frame using cbind. Be careful with
#' this function at present, as you can easily flood memory by choosing
#' columns with large numbers of unique values.
#'
#' @param df - a data frame
#' x - a single column name or number
#'
#' @return Returns a new data frame with c - 1 binary columns representing the
#' removed column, where c is the number of unique values. The modal value is
#' removed altogether, and used as the reference value. This function is the
#' base for flg.create, and thus it's not exported.
as.flgs <- function(df,x){


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


#' flg.create
#'
#' @description This function takes in a named list and returns a data frame
#' containing the binary transformation of each of the named variables.
#' This allows you to do batch the flag conversion of all factor variables in
#' a data set. Be careful with this function, as it's easy to flood memory by
#' batching more than one column with large numbers of unique entries.
#'
#' @param df - A data frame
#' name.list - A list of the columns to be transformed.
#'
#' @export
#'
#' @examples head(flg.create(iris, list("Species")))
flg.create <- function(df, name.list){


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
