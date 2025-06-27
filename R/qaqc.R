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
  dat <- llsbios:::get_data(channel = channel, sta_num = station)

  qaqc_plot_dat <- llsbios:::make_plot_data(depth = dat[[1]], position = dat[[2]], length = dat[[3]])

  make_plots(depth = qaqc_plot_dat[[1]], chx = qaqc_plot_dat[[2]], len3 = qaqc_plot_dat[[3]], testE = qaqc_plot_dat[[4]], testE = qaqc_plot_dat[[5]], StnLoc = qaqc_plot_dat[[6]])
}
