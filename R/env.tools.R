#' Title env.size
#' @description Get the size of an environment by typing in its name as as string.
#' @param env - The name of the environment, as a string.
#' @return
#' @export
env.size <- function(env){
  sum(env.list(env)$size)
}


#' env.list()
#' @description Get the size of all objects in an environment
#' @param env 
#'
#' @return
#' @export
env.list <- function(env){
  df <- ls(as.environment(env), all.names=T) %>% 
    sapply(function(x) object.size(get(x, envir=as.environment(env)))) %>%
    data.frame(attribute = names(.), size=.) %>%
    arrange(desc(size))
  
  df$class <- ls(as.environment(env), all.names=T) %>% 
    sapply(function(x) class(get(x, as.environment(env)))) %>%
    sapply(function(x) paste(x,collapse=","))
  
  rownames(df) <- NULL
  return(df %>% select(attribute, class,size))
}


#' mem.size()
#' @description Get a list of all environments and their size in memory. 
#' @return
#' @export
#'
#' @examples
mem.size <- function(){
  df <- search() %>% sapply(env.size) %>%
    data.frame(environment = names(.), size=.) %>%
    arrange(desc(size))

  rownames(df) <- NULL
  return(df)
}