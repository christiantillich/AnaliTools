
#' name.search
#' @description Iterates through the list of attributes of an object. Unlike
#' calling attributes, name.search works recursively. Really great for xml/html
#' returns where you want to get a sense of the full hierarchy.
#' @param x - Any object
#' @return names(x), but iterated recursively through every level of the object
#' names.
#' @export
name.search <- function(x, top=""){
  for(n in names(x)){
    print(paste(top,"$",n,sep=""))
    #print(names(x[[n]]))
    if(!is.null(names(x[[n]]))){
      name.search(x[[n]], top=paste("$",n,sep=""))
    }
  }
}


#' look.for
#' @description Looks for a single named attribute in a list of lists. Returns a
#' vector containing every instance of that attribute. Pretty handy for playing
#' with API call results.
#' @param list - a list of lists
#' @param target - name of the attribute.
#' @return a single vector containing the value of the t
#' @export
look.for <- function(list.list, target){
  sapply(list.list, function(x) tryCatch(x[[target]], error=function(e) NA))
}
