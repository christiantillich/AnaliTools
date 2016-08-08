#Custom knitr output
#' Knitrdone
#' @description Creates a wrapper around ezknit, which allows knitting to a
#' specific output directory without messing up the working directory /
#' environment varialbes. This wrapper just creates directories if none exist,
#' and it can be placed at the beginning of a .RMD doc as the custom knit
#' function.
#' @param dir - The directory to write the output doc and all intermediaries to.
#' @return Returns the .html and .md documents.
#' @export
#' @examples
knitrdone <- function(dir){
  function(inputFile, encoding)
  {
    if(!file.exists(dir)) {
      dir.create(dir)
    }

    ezknit(
      inputFile,
      out_dir = dir,
      keep_md = T,
      keep_html = T,
      verbose=T,
      chunk_opts = list(tidy=F, fig.width=9)
    )
  }
}
