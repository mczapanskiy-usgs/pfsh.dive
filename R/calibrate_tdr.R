calibrate_tdr <- function(tdr, id, rate = 0.1, surface_thr = .1, depth_thr = .2, dur_thr = .5) {
  if(nrow(tdr) == 0) {
    warning(sprintf('No data for TDR %s', id))
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
      mutate(DeployID = id)
  } else {
    NULL
  }
}
