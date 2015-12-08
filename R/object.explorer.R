
#Iterates through the list of attributes of an object. 
name.search <- function(x, top=""){
  for(n in names(x)){
    print(paste(top,"$",n,sep=""))
    #print(names(x[[n]]))
    if(!is.null(names(x[[n]]))){
      name.search(x[[n]], top=paste("$",n,sep=""))
    }
  }
}


#Looks for a single named object in a list of lists. 
look.for <- function(list, target){
  sapply(list, function(x) x[[target]])
}