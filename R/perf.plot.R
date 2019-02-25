library(plyr)
library(dplyr)
library(Rcpp)
library(ggplot2)
library(magrittr)


#' .buckets
#' @description - Does the automatic bucketing for the plot. Really just
#' cribbing on hist$breaks to do the heavy work, but with some better handling
#' of categorical variables. Also includes a switch to treat anything numeric
#' as categorical instead.
#' @param x - A vector to be bucketed.
#' @param num.switch - Binary. True if we should treat x as numeric, false if
#' categorical.
#' @return Returns a list object with 2 values. $groups is a vector containing
#' the distinct bucket values. $buckets returns the snapped values.
#' @examples .buckets(iris[,1], F)
#' .buckets(iris[,2])
.buckets <- function(
  x,
  num.switch = class(x) %in% c("numeric", "integer", "float")
 )
 {
  #If the variable is numeric and the switch is okay,
  if(num.switch){

    #Use the histogram function to get the groupings
    groups <- hist(x, plot=F)$breaks

    #Use the cute sapply stuff to do the lookup.
    buckets <- sapply(x, function(a) groups[which.min(abs(groups - a))]) %>% as.numeric

  }else{
  #If the variable is non-numeric, or the switch has been thrown.

    #If the variable has thirty or fewer unique values
    if(length(unique(x)) < 30){

      #The groupings are simply unique values as a character vector.
      groups <- unique(x)

      #The buckets are simply a character conversion of the vector.
      buckets <- x

    }else{
    #If the variable has more than thirty unique values

      #Take the first 29 most frequent values
      n <- table(x)[x %>% table() %>% order(decreasing = T)] %>% head(29) %>% names

      #Groupings are those 29 plus "other" category
      groups <- c(n, "Other")

      #Buckets are the vector, where the non-grouped are changed to Other.
      buckets <- x; buckets[!buckets %in% n] <- "Other"
    }
  #Return the buckets and the groupings.
  }
  return(list(groups=groups, buckets=buckets))
}
# .buckets(iris[,1], F)
# .buckets(iris[,2])



#' perf.plot
#' @description Creates a table for a common univariate analysis - the target variable
#' averaged at different levels of x. Table returns mean values and counts at the
#' different levels of x, where the levels are determined by using hist()$breaks.
#' @param y - The target variable
#' @param x - The independent variable.
#' @return Returns a dataframe with the mean value of y at bucketed values of x. 
#' @export
perf.table <- function(y, x,
  num.switch = class(x) %in% c("numeric", "integer", "float")
){
  #Makes the x-axis groupings for the variable.
  groups <- .buckets(x, num.switch)$groups
  buckets <- .buckets(x, num.switch)$buckets
  if(num.switch){buckets %<>% as.numeric}


  #This is the main data manipulation step. Gets summary stats by bucket.
  data.frame(x, buckets, y) %>%
    group_by(buckets) %>%
    summarise(count = n(), avg = mean(y, na.rm=T)) %>%
    merge(as.data.frame(groups), by.x="buckets", by.y="groups", all.y=T) %>%
    mutate(
       count = replace(count, is.na(count), 0)
      ,avg = replace(avg, is.na(avg), 0)
      )
}

#' perf.plot
#' @description Creates my most common univariate plot - the target variable
#' averaged at different levels of x and superimposed over a histogram showing
#' the population distribution across the different values of x. The returned
#' plot is a ggplot2 object.
#' @param y - The target variable
#' @param x - The independent variable.
#' @param xlab - x-axis label, straight from qplot.
#' @param ylab - y-axis label, straight from qplot.
#' @param main - Main title, straight from qplot.
#' @param num.switch - Binary. True if we should treat x as numeric, false if
#' categorical.
#' @param col - Color of the univariate line, straight from qplot.
#' @param lwd - Histogram width control. Should default to something sensible.
#' @return Returns a ggplot object with the mean value of y plotted over x,
#' and a histogram overlayed in the background.
#' @export
#' @examples perf.plot(as.integer(iris$Species == "virginica"),iris[,1])
#' perf.plot(as.integer(iris$Species == "virginica"),iris[,2])
#' perf.plot(as.integer(iris$Species == "virginica"),iris[,3])
#' perf.plot(as.integer(iris$Species == "virginica"),iris[,1], num.switch=FALSE)
#' perf.plot(as.integer(iris$Species == "virginica"),iris[,3], num.switch=FALSE)
perf.plot <- function(y, x
  ,xlab = deparse(substitute(x))
  ,ylab = deparse(substitute(y))
  ,main = paste(ylab, xlab, sep=" vs. " )
  ,num.switch = class(x) %in% c("numeric", "integer", "float")
  ,col = 'black'
  ,lwd = 450/length(.buckets(x, num.switch)$groups)
  ){

  #This is the main data manipulation step. Gets summary stats by bucket.
  t <- perf.table(y, x, num.switch)

  ggplot(
    data = t, mapping = aes(
       x = buckets
      # ,y = avg
    )
  ) + theme_BA() +
      geom_point(aes(y=avg), color=I(col)) +
      geom_bar(
       aes(y=count * max(abs(avg))/max(count))
       ,stat="identity"
       ,fill=I("grey")
       ,alpha = I(0.50)
      ) +
     geom_bar(
       aes(y=-count * max(abs(avg))/max(count))
      ,stat="identity"
      ,fill=I("grey")
      ,alpha = I(0.50)
    )  +
    if(num.switch){
      geom_line(aes(y=avg), color=I(col))
    }
}

# perf.plot(as.integer(iris$Species == "virginica"),iris[,1])
# perf.plot(as.integer(iris$Species == "virginica"),iris[,2])
# perf.plot(as.integer(iris$Species == "virginica"),iris[,3])
# perf.plot(as.integer(iris$Species == "virginica"),iris[,1], num.switch=F)
# perf.plot(as.integer(iris$Species == "virginica"),iris[,3], num.switch=F)

#TODO: Add some data to test that's negative.
