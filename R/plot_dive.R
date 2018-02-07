#' Create a dive plot
#'
#' \code{plot_dive} creates a plot of a single dive in two panels. The left
#' panel zooms in on the dive itself and the right panel is an overview of the
#' entire event. In the left panel, blue points are within the dive and red
#' points are the surrounding records for context. The right panel has two
#' curves. The red one shows the original, pre-calibration points. The green
#' one is the calibrated dive with the surface offset applied. The shaded
#' region delineates the dive itself. In both panels, the dashed blue line
#' indicates the surface noise threshold.
#'
#' @param calib_tdr data.frame. Use \code{\link{calibrate_tdr}}.
#' @param diveid integer. Identifier of dive to plot.
#' @param surface_thr numeric. Threshold for surface noise.
#'
#' @import ggplot2
#'
#' @return gtable with two plots.
#'
#' @examples
#' # Load metadata
#' metadata_path <- system.file('extdata',
#'                              'MOC2015PFSHmetadata.csv',
#'                              package = 'pfsh.dive')
#' metadata <- readr::read_csv(metadata_path)
#'
#' # Read CEFAS output
#' tdr_path <- system.file('extdata',
#'                          paste0(metadata$TDR_filename[1], '.CSV'),
#'                          package = 'pfsh.dive')
#' tdr <- read_cefas(tdr_path,
#'                   metadata$Deployed[1],
#'                   metadata$Recovered[1])
#'
#' # Calibrate TDR data
#' calib_tdr <- calibrate_tdr(tdr, metadata$DeployID[1])
#'
#' # Plot a dive
#' plot_dive(calib_tdr, 4)
#'
#' @export
plot_dive <- function(calib_tdr, diveid, surface_thr = .1) {
  # Event and dive details
  dive <- filter(calib_tdr, DiveID == diveid)
  eventid <- dive$EventID[1]
  event <- filter(calib_tdr, EventID == eventid)
  deployid <- calib_tdr$DeployID[1]
  dive_begin <- first(dive$UTC)
  dive_end <- last(dive$UTC)

  # Dive plot
  dive_press_lim <- range(dive$CalibPressure, na.rm = TRUE) %>%
    pmin(c(0, Inf)) %>%
    rev
  expand_sec <- 2
  break_fun <- function(lim) seq(lim[1], lim[2], by = 0.5)
  dive_plot <- event %>%
    filter(between(UTC, dive_begin - expand_sec, dive_end + expand_sec)) %>%
    mutate(InDive = factor(!is.na(DiveID) & DiveID == diveid)) %>%
    ggplot(aes(x = UTC, y = CalibPressure)) +
    geom_line(color = 'blue') +
    geom_point(aes(color = InDive)) +
    geom_hline(yintercept = surface_thr,
                        color = 'blue',
                        linetype = 'dashed') +
    ylim(dive_press_lim) +
    scale_x_datetime(date_labels = '%H:%M:%OS1',
                              breaks = break_fun) +
    scale_color_manual(values = c('red', 'blue')) +
    theme(legend.position = 'none') +
    labs(x = 'Time (UTC)',
                  y = 'Depth (m)',
                  title = sprintf('DeployID %s DiveID %s', deployid, diveid))

  # Event plot
  event_press_lim <- range(dive$Pressure,
                           dive$CalibPressure,
                           na.rm = TRUE) %>%
    pmin(c(0, Inf)) %>%
    rev
  event_plot <- event %>%
    mutate(InDive = factor(!is.na(DiveID) & DiveID == diveid)) %>%
    ggplot(aes(x = UTC)) +
    geom_line(aes(y = Pressure), color = 'red') +
    geom_point(aes(y = Pressure), color = 'red') +
    geom_line(aes(y = CalibPressure), color = 'green') +
    geom_point(aes(y = CalibPressure), color = 'green') +
    geom_hline(yintercept = surface_thr,
                        color = 'blue',
                        linetype = 'dashed') +
    annotate('rect',
                      xmin = dive_begin,
                      xmax = dive_end,
                      ymin = event_press_lim[2],
                      ymax = event_press_lim[1],
                      alpha = 0.2,
                      fill = 'black') +
    ylim(event_press_lim) +
    scale_x_datetime(date_labels = '%H:%M:%OS1',
                              breaks = break_fun) +
    labs(x = 'Time (UTC)',
                  y = 'Depth (m)',
                  title = '')

  # Dive and event plots side-by-side
  gridExtra::grid.arrange(dive_plot, event_plot, ncol = 2, widths = c(3, 2))
}
