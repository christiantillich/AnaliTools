#' FigR
#'
#' @description Class constructor for image handling. FigR has a path value
#' telling it where to print out, a method to add an image, a method to delete
#' an image, and a method to print out all images to the path.
#' @param write.path - Path for writing out all the collected images. Defaults
#' to the working directory.
#' @param index - FigR will append the phrase 'figXXX' to the name of the file.
#' Index determines the value the starting value. So the first three files
#' printed out by FigR will have names 'fig001...', 'fig002...', and 'fig003...'.
#' Defaults to 1
#' @param type - file type to save as, as text. Defaults to .png.
FigR <- function(write.path = getwd(), index = 1, type='.png'){

  #The object.
  me <- list(
     images = list()
    ,write.path = write.path
    ,index=index
    ,type=type
  )

  class(me) <- 'FigR'
  return(me)
}

#Method to add image
add.image <- function(self, image, name) UseMethod('add.image',self)
add.image.default <- function(self, image, name){
  stop('Argument 1 must be of class FigR')
}
add.image.FigR <- function(self, image, name){

  #Error if image is not a ggplot object.
  if(not('ggplot' %in% class(image))){stop('Added image must be ggplot object')}

  #Add the image to the container
  cursor <- length(self$images) + 1;
  self$images[cursor] <- list(image)
  names(self$images)[cursor] <- name

  return(self)
}

#Method to remove image
delete.image <- function(self, name.or.id) UseMethod('delete.image', self)
delete.image.default <- function(self, name.or.id){
  stop('Argument 1 must be of class FigR')
}
delete.image.FigR <- function(self, name.or.id){
  self$images[name.or.id] <- NULL
  return(self)
}

#Method to print all images to write.path
write.all <- function(self) UseMethod('write.all',self)
write.all.default <- function(self){
  stop('Argument 1 must be of class FigR')
}
write.all.FigR <- function(self){
  if(length(self$images) == 0){stop('No plots to write. Please add some.')}

  i <- self$index

  helper <- function(x){
    name <- paste0('/fig', formatC(self$index, digits=2, flag=0),'_',x)
    name <- paste0(self$write.path, name, self$type)
    print(paste("Printing", name))
    qsave(name,plot=self$images[[x]])
    i <- i + 1
  }

  lapply(names(test$images), helper)
  return()
}




# test <- FigR()
# p1 <- qplot(1:100)
# p2 <- qplot(100:200)
# test <- add.image(test, p1, 'plot_1')
# test <- add.image(test, p2, 'plot_2')
# test <- delete.image(test, 1)
# write.all(test)
# test <- delete.image(test, 'plot_2')
# write.all(test)
