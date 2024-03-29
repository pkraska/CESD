% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/COERS_adcp_proc.R
\name{ADCP_proc}
\alias{ADCP_proc}
\title{Converting ADCP ensembles into a header and ensemble data tables}
\usage{
ADCP_proc(x)
}
\arguments{
\item{x}{text file containing ADCP data from either a Teledyne Riverray units
with a 6 line header and a single extra line at the start of the data.}
}
\value{
A list composed of the raw data, the flattened header information, as
  well as the actual ensemble data.

list of the raw, header, and body data.
}
\description{
Converting ADCP ensembles into a header and ensemble data tables
}
\examples{
ADCP_proc(x)

}
