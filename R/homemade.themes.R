
library(grid)
library(ggthemes)


#' theme_BA()
#' @description Creates my go-to theme for the ggplot in analyses.
#' @return ggplot theme object, based off ggthemes::theme_gdocs()
#' @export
theme_BA <- function(){
  x <- ggthemes::theme_gdocs()
  x$plot.title$size = 25
  x$plot.title$hjust = 0.5
  x$plot.title$vjust = 4
  x$axis.text.x$angle = 45
  x$axis.title.y$angle = 90
  x$plot.margin = grid::unit(c(0.1,0.1,0.1,0.1), 'npc')
  x$axis.title.x$vjust = -1
  x$axis.title.y$vjust = 2.5

  return(x)
}

#' qsave
#' @description Just a ggsave wrapper but with presaved settings
#' @param filename - file name/filename of plot
#' @param height - ggsave input. Defaults to 6.
#' @param width - ggsave input. Defaults to 9.
#' @param dpi - ggsave input. Defaults to 600.
#' @export
qsave <- function(filename,height=6, width=9,dpi=600,...) {
  ggsave(filename,height=height, width=width,dpi=dpi,...)
}
