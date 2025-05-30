% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_max_eto_at_location.R
\name{get_max_eto_at_location}
\alias{get_max_eto_at_location}
\title{Get Max Reference Evapotranspiration Values by Geographic Location}
\usage{
get_max_eto_at_location(img, lat, long)
}
\arguments{
\item{img}{SpatRaster with the maximum reference evapotranspiration (ETo) grid downloaded from max_eto_grid_download.}

\item{lat}{Numeric. Latitude of the location in decimal degrees.}

\item{long}{Numeric. Longitude of the location in decimal degrees.}
}
\value{
A `data.frame` object containing the maximum reference evapotranspiration (ETo) values per pair of coordinates.
}
\description{
Extract maximum reference evapotranspiration (max ETo) values for one or more locations of interest, using data from the dataset provided by Dias (2018).
}
\examples{
\dontrun{
# Visualize Brazilian states (optional)

max_eto_df <- get_max_reference_eto_by_location(img, 
                                                long = c(-51.95, -43.23),
                                                lat = c(-23.52, -21.34))
}

}
\author{
Roberto Filgueiras.
}
