% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.tools.R
\name{l.cap}
\alias{l.cap}
\title{l.cap}
\usage{
l.cap(v, lbnd = -Inf, ubnd = Inf)
}
\arguments{
\item{v}{- A list input.}

\item{lbnd}{- The lower bound. Defaults to -Inf.}

\item{ubnd}{- The upper bound. Defaults to Inf}
}
\value{
Returns the list, where values are replaced by the bound if they
exceed that limit.
}
\description{
An lapply wrapper for cap.
}
