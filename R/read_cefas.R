#' Read a CEFAS file
#'
#' Downloading data from CEFAS tags yields a non-tabular CSV. This reads a
#' CEFAS file and extracts the depth records in a tidy data.frame.
#'
#' @param cefas_file character. Path to CEFAS file.
#' @param deployed POSIXct. Datetime of deployment.
#' @param recovered POSIXct. Datetime of recovery
#'
#' @return data.frame with columns
#' \itemize{
#'   \item{\code{Row} (Row number in original CEFAS file)}
#'   \item{\code{UTC} (Datetime of record in UTC timezone)}
#'   \item{\code{Pressure}}
#'   \item{\code{EventID} (FastLog identifier)}
#' }
#'
#' @examples
#' TODO
#'
#' @export
read_cefas <- function(cefas_file, deployed, recovered) {
  # Open CEFAS file
  raw_cefas <- readLines(cefas_file)

  # NOTE ON EVENT IDS
  # For Fast Log points, event id corresponds to index of Fast Log i.e. 1, 2, ..., count(FastLogs)
  # Data Block 0 points have event id 0
  # Example Data Block 0 data point: 13/06/14 12:00:00,-0.82
  # Example Fast Log data point: 15/06/14 06:45:42.100,-1.16

  record_regex <- '[[:digit:]/]{8} [[:digit:]:]{8}[[:digit:].]*,[-[:digit:].]+'
  record_df <- data.frame(Row = grep(record_regex, raw_cefas))

  ### Data Block 0 + Fast Logs produce points with pressure readings (explicit points) ###
  # NOTE: POSIXct handles fractional seconds unintuitively. Adding .01 is a workaround to ensure the
  # correct value.

  # Verify file has data
  if(nrow(record_df) == 0) {
    warning(sprintf('CEFAS file %s contains no records', cefas_file))
    return(NULL)
  }

  # Extract date/time and pressure
  dtp_df <- record_df %>%
    mutate(raw_record = raw_cefas[Row]) %>%
    separate(raw_record, c('UTC', 'Pressure'), ',') %>%
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%d/%m/%y %H:%M:%OS') + 0.01,
           Pressure = as.double(Pressure))

  # Event IDs
  event_bounds <- data.frame(start_row = grep("Data points available", raw_cefas)) %>%
    mutate(EventID = row_number() - 1)
  find_EventID <- function(row) {
    event_row <- findInterval(row, event_bounds$start_row)
    event_bounds$EventID[event_row]
  }
  event_df <- dtp_df %>%
    mutate(EventID = find_EventID(Row))

  # Sort chronologically, filter dates to deployment, and return data frame
  event_df %>%
    arrange(UTC) %>%
    filter(between(UTC, deployed, recovered))
}
