% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_dives.R
\name{summarize_dives}
\alias{summarize_dives}
\title{Summarize dives in calibrated TDR data}
\usage{
summarize_dives(calib_tdr)
}
\arguments{
\item{calib_tdr}{data.frame. Use \code{\link{calibrate_tdr}}.}
}
\value{
data.frame with columns
\itemize{
  \item{\code{DeployID}}
  \item{\code{DiveID}}
  \item{\code{Begin} (in UTC)}
  \item{\code{End} (in UTC)}
  \item{\code{Depth} (in meters)}
  \item{\code{Duration} (in seconds)}
}
}
\description{
\code{summarize_dives} provides the depth and duration of all the dives in
calibrated TDR data. Note: Begin and End times may be to sub-second
accuracy and default R print options conceal decimal values.
}
\examples{
# Load metadata
metadata_path <- system.file('extdata',
                             'MOC2015PFSHmetadata.csv',
                             package = 'pfsh.dive')
metadata <- readr::read_csv(metadata_path)

# Read CEFAS output
tdr_path <- system.file('extdata',
                         paste0(metadata$TDR_filename[1], '.CSV'),
                         package = 'pfsh.dive')
tdr <- read_cefas(tdr_path,
                  metadata$Deployed[1],
                  metadata$Recovered[1])

# Calibrate TDR data
calib_tdr <- calibrate_tdr(tdr, metadata$DeployID[1])

# Summarize dives in a table
summarize_dives(calib_tdr)

}
