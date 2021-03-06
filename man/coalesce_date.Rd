% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coalesce cols.R
\name{coalesce_date}
\alias{coalesce_date}
\title{Coalesce Date columns}
\usage{
coalesce_date(...)
}
\arguments{
\item{...}{Vectors}
}
\value{
A date vector
}
\description{
Takes any number of vectors as input, coerces to character vectors and
uses dplyr::coalesce to coalesce them, then converts charactor vector
to Date vector
}
