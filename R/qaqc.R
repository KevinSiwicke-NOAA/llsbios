#' Run QAQC of AFSC longline survey station
#'
#' @param channel channel to access database
#' @param station station number
#'
#' @return makes plots for qaqc
#' @export
#'
#' @examples
qaqc <- function(channel, station) {
  dat <- qaqc:::get_data(channel = channel, sta_num = station)

  plot_dat <- qaqc:::make_plot_data(depth = dat[[1]], position = dat[[2]], length = dat[[3]])

  make_plots(depth = plot_dat[[1]], chx = plot_dat[[2]], len3 = plot_dat[[3]], testE = plot_dat[[4]], testE = plot_dat[[5]], StnLoc = plot_dat[[6]])
}
