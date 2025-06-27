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

  plot_dat <- llsbios:::make_plot_data(depth = dat[[1]], position = dat[[2]], length = dat[[3]])

  make_plots(sta = station, depth = plot_dat[[1]], chx = plot_dat[[2]], lengths = plot_dat[[3]], lenE = plot_dat[[4]], lenO = plot_dat[[5]], StnLoc = plot_dat[[6]], stnd = dat[[4]])
}


#' Check TDR output and get temperatures
#'
#' @param station station number
#'
#' @return saved plots
#' @export
#'
#' @examples
tdr <- function(station) {
  tdr_dat <- utils::read.table(paste("TDR/Raw/sta", station, ".asc", sep = ""), skip = 50, header = TRUE, sep = ",")
  colnames(tdr_dat) <- c("Temperature","Depth","Date","Time")

  tdr_dat$Diff <- c(0,diff(tdr_dat$Depth))
  tdr_dat$Time <- as.POSIXct(strptime(tdr_dat$Time, format = "%H:%M:%S"))
  bot <- as.data.frame(tdr_dat[abs(tdr_dat$Diff) <= 0.03 & tdr$Depth > (max(tdr$Depth) - 30), ])
  bot <- bot[abs(bot$Depth - mean(bot$Depth)) <= 1.5, ]

  Year <- lubridate::year(bot$Time[1])
  Julian <- lubridate::yday(bot$Time[1])
  Station <- station
  Depth <- round(mean(bot$Depth), 1)
  MeanTemp <- round(mean(bot$Temperature), 2)
  MinTemp <- round(min(bot$Temperature), 2)
  MaxTemp <- round(max(bot$Temperature), 2)
  NumScans <- nrow(bot)

  ####
  # NOW MAKE A PROFILE OF THE DOWNCAST/UPCAST
  ####

  updat <- tdr_dat[tdr_dat$Diff< -1.2 & tdr_dat$Depth > 0,]
  updat$TimeDiff <- updat$Time-mean(updat$Time)
  units(updat$TimeDiff) <- "mins"
  updat <- updat[abs(updat$TimeDiff) < 40, ]

  dodat <- tdr_dat[tdr_dat$Diff>1.2 & tdr_dat$Depth > 0, ]
  dodat$TimeDiff <- dodat$Time - mean(dodat$Time)
  units(dodat$TimeDiff) <- "mins"
  dodat <- dodat[abs(dodat$TimeDiff) < 40, ]

  # CREATE TWO PLOTS
  p1 <- ggplot2::ggplot(tdr_dat, ggplot2::aes(Time, Depth)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = bot, ggplot2::aes(x = Time, y = Depth), color = "red") +
    ggplot2::geom_point(data = updat, ggplot2::aes(x = Time, y = Depth), color = "blue") +
    ggplot2::geom_point(data = dodat, ggplot2::aes(x = Time, y = Depth), color = "orange") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_reverse() +
    ggplot2::ggtitle(paste0("Station #", station, ", Depth = ", Depth, ", Gear Temperature = ", MeanTemp)) +
    ggplot2::theme(plot.title = element_text(color = "red"))

  p2 <- ggplot2::ggplot(data = updat, ggplot2::aes(x = Temperature, y = Depth)) +
    ggplot2::geom_path() +
    ggplot2::geom_point(shape = 21, color = "blue") +
    ggplot2::geom_path(data = dodat, ggplot2::aes(x = Temperature, y = Depth)) +
    ggplot2::geom_point(data = dodat, ggplot2::aes(x = Temperature, y = Depth), shape = 21, color = "orange") +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_bw()

  tdrCheck <- cowplot::plot_grid(p1, p2, align = 'v')

  ggplot2::ggsave(tdrCheck, file=paste0("TDR/Today/Sta",station,"_TDRcheck.png"))

  saveRDS(tdr_dat, file=paste0("TDR/Profile/sta_",station,"_tdr.Rds"))
}
