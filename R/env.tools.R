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
    sapply(function(x) tryCatch(get(x, envir=as.environment(env)),error = function(e){NA})) %>%
    sapply(function(x) tryCatch(object.size(x), error=function(e){0})) %>% 
    data.frame(attribute = names(.), size=.) %>%
    arrange(desc(size))
  
  df$class <- df$attribute %>% 
    lapply(function(x) tryCatch(class(get(x, as.environment(env))), error=function(e){NA})) %>%
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

#' mem.trace
#' @description Trace the stack and see the memory size at each level. 
#' @return A dataframe containing
#' @export
#'
#' @examples
# mem.trace <- function(){
#     print(sapply(sys.frames(), env.size))
#     data.frame(
#        call = substr(as.character(unlist(sys.calls())), 1,30)
#       ,size = sapply(sys.frames(), env.size)
#     )
# }