% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farsr.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Generate a summary (counts per year) for the FARS data}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A list of numbers (years).}
}
\value{
Summary of accidents' counts by year (data frame).
}
\description{
\code{fars_summarize_years} reads the FARS data from disk for input years and
generates a summary.
}
\examples{
\donttest{
fars_summarize_years(years = c(2013,2014))
fars_summarize_years(2015)
}

}
