#' Calibrate CEFAS data
#'
#' \code{calibrate_tdr} is a wrapper for
#' \code{\link{diveMove::calibrateDepth}}. CEFAS tags record depth in bursts,
#' so this function uses a split-apply-combine approach to calibrate each
#' FastLog individually. Tags have an error of +/-1m, so for shallow diving
#' birds the error may be greater than the depth of the dive. To avoid losing
#' these dives, the "surface" is assumed to be at the shallowest point in each
#' FastLog, not at 0. For tags with a 1m threshold, this may bias dive depths
#' shallower.
#'
#' @param tdr data.frame. Use \code{\link{read_cefas}} to parse CEFAS file.
#' @param id character. Deployment identifier.
#' @param rate numeric. Sampling rate for FastLog events.
#' @param surface_thr numeric. Threshold for surface noise.
#' @param depth_thr numeric. Minimum depth to qualify as a dive.
#' @param dur_thr numeric. Minimum duration to qualify as a dive.
#'
#' @return data.frame with columns
#' \itemize{
#'   \item{\code{Row} (Row number in original CEFAS file)}
#'   \item{\code{UTC} (Datetime of record in UTC timezone)}
#'   \item{\code{Pressure}}
#'   \item{\code{EventID} (FastLog identifier)}
#'   \item{\code{CalibPressure} (Pressure after surface offset)}
#'   \item{\code{Surface} (Surface offset)}
#'   \item{\code{DivePhase} (From diveMove)}
#'   \item{\code{duration}}
#'   \item{\code{depth}}
#'   \item{\code{DiveID}}
#'   \item{\code{DeployID}}
#' }
#'
#' @examples
#' # Uses tidyverse functions
#' metadata <- system.file('extdata',
#'   'MOC2015PFSHmetadata.csv',
#'   package = 'pfsh.dive') %>%
#'   read_csv
#' example_tdr <- system.file('extdata',
#'   paste0('metadata$TDR_filename[1]', '.CSV'),
#'   package = 'pfsh.dive')
#' tdr <- read_cefas(example_tdr,
#'   metadata$Deployed[1],
#'   metadata$Recovered[1])
#' calib_tdr <- calibrate_tdr(tdr, metadata$DeployID[1])
#'
#' @export
calibrate_tdr <- function(tdr, deployid, rate = 0.1, surface_thr = .1, depth_thr = .2, dur_thr = .5) {
  if(nrow(tdr) == 0) {
    warning(sprintf('No data for TDR %s', deployid))
    return(tdr)
  }

  # Filter out events shorter than duration threshold or depth range less than dive threshold
  valid_events <- tdr %>%
    filter(EventID > 0) %>%
    group_by(EventID) %>%
    summarize(duration = as.numeric(max(UTC, na.rm = TRUE) - min(UTC, na.rm = TRUE), units = 'secs'),
              depthRange = max(Pressure, na.rm = TRUE) - min(Pressure, na.rm = TRUE),
              N = n()) %>%
    filter(duration >= dur_thr,
           depthRange >= depth_thr,
           N > 4)

  # Utility function for calibrating individual events
  calibrate_event <- function(event) {
    surface <- min(event$Pressure, na.rm = TRUE)
    # Create a TDR object
    calib_event <- createTDR(event$UTC,
                             event$Pressure,
                             dtime = rate,
                             file = 'NA') %>%
      # Calibrate event using minimum pressure as surface offset
      calibrateDepth(wet.thr = 0,
                     dive.thr = surface_thr,
                     zoc.method = 'offset',
                     offset = surface)

    # Pull calibrated pressures, initial dive ids, and phases from calibrated TDR object
    calib_event_df <- data.frame(CalibPressure = calib_event@tdr@depth,
                                 Surface = surface,
                                 DiveIDinit = calib_event@dive.activity$dive.id,
                                 DivePhase = calib_event@dive.phases)

    # Bind original event data with calibrated data
    cbind(event, calib_event_df) %>%
      arrange(UTC)
  }

  # Split-apply-combine to calibrate events
  if(nrow(valid_events) > 0) {
    calib_events <- semi_join(tdr, valid_events, by = 'EventID') %>%
      group_by(EventID) %>%
      do(calibrate_event(.))

    # Re-assign dive IDs in consecutive order
    dive_ids <- calib_events %>%
      filter(DiveIDinit > 0) %>%
      group_by(EventID, DiveIDinit) %>%
      summarize(duration = as.numeric(max(UTC, na.rm = TRUE) - min(UTC, na.rm = TRUE), units = 'secs'),
                depth = max(CalibPressure, na.rm = TRUE)) %>%
      filter(duration >= dur_thr,
             depth >= depth_thr) %>%
      ungroup %>%
      mutate(DiveID = row_number())

    calib_events %>%
      left_join(dive_ids, by = c('EventID', 'DiveIDinit')) %>%
      select(-DiveIDinit) %>%
      mutate(DeployID = deployid)
  } else {
    NULL
  }
}
