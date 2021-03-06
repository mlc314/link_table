% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/link_table_functions.R
\name{GEOS_area}
\alias{GEOS_area}
\title{Compute area using GEOS (rgeos)}
\usage{
GEOS_area(polys, ...)
}
\arguments{
\item{polys}{(sf) sf object to compute areas over}

\item{...}{remaining arguments for rgeos::gArea()}
}
\value{
(numeric) areas
}
\description{
sf::st_area does some approximation in its computation.
rgeos::gArea is more consistent.
}
