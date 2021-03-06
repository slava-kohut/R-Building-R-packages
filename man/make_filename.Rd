% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farsr.R
\name{make_filename}
\alias{make_filename}
\title{Construct a filename (string) for a subset of the FARS data.}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A single number (integer or numeric).}
}
\value{
A filename (single string)
}
\description{
\code{make_filename} takes a number (\code{year}, integer or numeric) on input and appends it
to a filename string. An error will be raised if \code{year} cannot be coerced to an integer.
}
\examples{
make_filename(year = 2013)
make_filename(2014L)

}
