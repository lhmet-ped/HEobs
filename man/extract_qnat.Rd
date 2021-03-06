% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qnat_data.R
\name{extract_qnat}
\alias{extract_qnat}
\title{Extract natural stramflow data from a ONS station and save to RDS file}
\usage{
extract_qnat(
  qnat_file = NA,
  stn_id = 74,
  save = TRUE,
  prefix = "qnat-obs-posto-",
  dest_dir = "output"
)
}
\arguments{
\item{qnat_file}{Character. Path to ascii data file (or a URL).}

\item{stn_id}{integer, station code from ONS station (output from
\code{info_station()[["posto"]]}).}

\item{save}{logical, TRUE to export data to RDS file.}

\item{prefix}{prefix to RDS file}

\item{dest_dir}{a character with the name of where the RDS file is
saved. Default: \code{fusepoc-prep/output}.}
}
\value{
a tibble with columns \code{date}, \code{posto}, \code{qnat} (cmecs).
}
\description{
Extract natural stramflow data from a ONS station and save to RDS file
}
\examples{
if(FALSE){
 qnat_posto <- extract_qnat(
                 qnat_file = NA,
                 stn_id = 74,
                 save = FALSE
                 )
 str(qnat_posto)
}
}
\seealso{
import_qnat
}
