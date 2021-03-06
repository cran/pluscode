% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pluscode_encode.R
\name{pluscode_encode}
\alias{pluscode_encode}
\title{Pluscode Encoder}
\usage{
pluscode_encode(lats, lngs, precision)
}
\arguments{
\item{lats}{A valid latitude within the range -90 to 90}

\item{lngs}{A valid latitude within the range -180 to 180}

\item{precision}{Options are 2, 4, 8, or 10}
}
\description{
This package retrieves a pluscode by inputting latitude and longitude
}
\examples{
pluscode_encode(52.501450, -6.567180, 8)

}

