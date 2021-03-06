% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perf.plot.R
\name{perf.plot}
\alias{perf.plot}
\title{perf.plot}
\usage{
perf.plot(y, x, xlab = deparse(substitute(x)),
  ylab = deparse(substitute(y)), main = paste(ylab, xlab, sep =
  " vs. "), num.switch = class(x) \%in\% c("numeric", "integer",
  "float"), col = "black", lwd = 450/length(.buckets(x,
  num.switch)$groups))
}
\arguments{
\item{y}{- The target variable}

\item{x}{- The independent variable.}

\item{xlab}{- x-axis label, straight from qplot.}

\item{ylab}{- y-axis label, straight from qplot.}

\item{main}{- Main title, straight from qplot.}

\item{num.switch}{- Binary. True if we should treat x as numeric, false if
categorical.}

\item{col}{- Color of the univariate line, straight from qplot.}

\item{lwd}{- Histogram width control. Should default to something sensible.}
}
\value{
Returns a ggplot object with the mean value of y plotted over x,
and a histogram overlayed in the background.
}
\description{
Creates my most common univariate plot - the target variable
averaged at different levels of x and superimposed over a histogram showing
the population distribution across the different values of x. The returned
plot is a ggplot2 object.
}
\examples{
perf.plot(as.integer(iris$Species == "virginica"),iris[,1])
perf.plot(as.integer(iris$Species == "virginica"),iris[,2])
perf.plot(as.integer(iris$Species == "virginica"),iris[,3])
perf.plot(as.integer(iris$Species == "virginica"),iris[,1], num.switch=FALSE)
perf.plot(as.integer(iris$Species == "virginica"),iris[,3], num.switch=FALSE)
}
