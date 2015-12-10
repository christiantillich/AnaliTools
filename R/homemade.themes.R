
require(grid)
require(ggthemes)


#' theme_BA()
#' @description Creates my go-to theme for the ggplot in analyses.
#' @return ggplot theme object, based off ggthemes::theme_gdocs()
#' @export
theme_BA <- function(){
  x <- theme_gdocs()
  x$plot.title$size = 25
  x$plot.title$hjust = 0.5
  x$plot.title$vjust = 4
  x$axis.text.x$angle = 45
  x$axis.title.y$angle = 90
  x$plot.margin = unit(c(0.1,0.1,0.1,0.1), 'npc')
  x$axis.title.x$vjust = -1
  x$axis.title.y$vjust = 2.5
}
