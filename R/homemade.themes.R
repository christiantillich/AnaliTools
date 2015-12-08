####Creating a specific theme for the plots in this analysis.
require(grid)
require(ggthemes)
theme_BA <- theme_gdocs()
theme_BA$plot.title$size = 25
theme_BA$plot.title$hjust = 0.5
theme_BA$plot.title$vjust = 4
theme_BA$axis.text.x$angle = 45
theme_BA$axis.title.y$angle = 90
theme_BA$plot.margin = unit(c(0.1,0.1,0.1,0.1), 'npc')
theme_BA$axis.title.x$vjust = -1
theme_BA$axis.title.y$vjust = 2.5
