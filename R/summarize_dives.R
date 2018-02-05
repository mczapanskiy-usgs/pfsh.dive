#' Summarize dives in calibrated TDR data
#'
#' \code{summarize_dives} provides the depth and duration of all the dives in
#' calibrated TDR data. Note: Begin and End times may be to sub-second
#' accuracy and default R print options conceal decimal values.
#'
#' @param calib_tdr data.frame. Use \code{\link{calibrate_tdr}}.
#'
#' @return data.frame with columns
#' \itemize{
#'   \item{\code{DeployID}}
#'   \item{\code{DiveID}}
#'   \item{\code{Begin} (in UTC)}
#'   \item{\code{End} (in UTC)}
#'   \item{\code{Depth} (in meters)}
#'   \item{\code{Duration} (in seconds)}
#' }
#'
#' @examples
#' # Uses tidyverse functions
#' metadata <- system.file('extdata',
#'   'MOC2015PFSHmetadata.csv',
#'   package = 'pfsh.dive') %>%
#'   read_csv
#' example_tdr <- system.file('extdata',
#'   paste0(metadata$TDR_filename[1], '.CSV'),
#'   package = 'pfsh.dive')
#' tdr <- read_cefas(example_tdr,
#'   metadata$Deployed[1],
#'   metadata$Recovered[1])
#' calib_tdr <- calibrate_tdr(tdr, metadata$DeployID[1])
#' summarize_dives(calib_tdr)
#'
#' @export
summarize_dives <- function(calib_tdr) {
  calib_tdr %>%
    filter(!is.na(DiveID)) %>%
    group_by(DeployID, DiveID) %>%
    summarize(Begin = min(UTC),
              End = max(UTC),
              Depth = max(CalibPressure),
              Duration = as.numeric(End - Begin, units = 'secs')) %>%
    ungroup
}
