#' Make QAQC plots
#'
#' @param sta station number
#' @param depth depths of each skate of gear
#' @param chx check that start and ends are near skate depths
#' @param lengths sablefish length data
#' @param lenE even lengths
#' @param lenO odd lengths
#' @param StnLoc station locations to compare with standard setting positions
#' @param stnd standard setting positions by station
#'
#' @return saved plots
#' @export
#'
#' @examples
make_plots <- function(sta, depth, chx, lengths, lenE, lenO, StnLoc, stnd) {
  Deps <- ggplot2::ggplot(data = depth, ggplot2::aes(x = hachi, y = intrpdep, color = factor(haul))) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = chx, ggplot2::aes(x = hachi, y = depth), color = "black", shape = 4, size = 3) +
    ggplot2::scale_y_reverse() +
    ggplot2::xlab("Skate #") +
    ggplot2::ylab("Depth (m)") +
    ggplot2::ggtitle(paste("Station", sta)) +
    ggplot2::theme_bw()

  ggplot2::ggsave(Deps, file=paste0("QAQC/Today/Sta",sta,"_DepthCheck.png"))

  if(length(unique(lengths$Haul)) == 2) {
    hist <-  ggplot2::ggplot(data = lenE, ggplot2::aes(x = Length, weight = Tot, fill = Sex)) +
      ggplot2::geom_histogram(binwidth = 1) +
      ggplot2::geom_histogram(data = lenO, ggplot2::aes(x = Length, weight = Tot, fill = Sex), binwidth=1, alpha=0.5) +
      ggplot2::geom_hline(yintercept = 0, col = "grey80", size = 0.5) +
      ggplot2::scale_fill_manual(values = c("red", "blue")) +
      ggplot2::xlab("Length (cm)") +
      ggplot2::ylab("Number of Sablefish") +
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::facet_wrap(~Haul) +
      ggplot2::ggtitle(paste("Station", sta, "- Dark = Even, Light = Odd")) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                     axis.text = ggplot2::element_text(size = 12),
                     strip.background = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 12))
  }

  if(length(unique(lengths$Haul)) == 1) {
    hist <- ggplot2::ggplot(data = lenE, ggplot2::aes(x = Length, weight = Tot, fill = Sex)) +
      ggplot2::geom_histogram(binwidth = 1) +
      ggplot2::geom_histogram(data = lenO, ggplot2::aes(x = Length, weight = Tot, fill = Sex), binwidth = 1, alpha = 0.5) +
      ggplot2::geom_hline(yintercept = 0, col = "grey80", size = 0.5) +
      ggplot2::scale_fill_manual(values = c("red", "blue")) +
      ggplot2::xlab("Length (cm)") +
      ggplot2::ylab("Number of Sablefish") +
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::ggtitle(paste("Station", sta, "- Dark = Even, Light = Odd")) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                     axis.text = ggplot2::element_text(size = 12),
                     strip.background = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 12))
  }

  ggplot2::ggsave(hist, file = paste0("QAQC/Today/Sta", sta, "_LenHist.png"))

  location <- ggplot2::ggplot() +
    ggplot2::geom_point(data = stnd, ggplot2::aes(x = Lon, y = Lat), col = "gray") +
    ggplot2::geom_point(data = StnLoc, ggplot2::aes(x = StartLon, y = StartLat, ), col = "blue", size = 2) +
    ggplot2::geom_point(data = StnLoc, ggplot2::aes(x = EndLon, y = EndLat), col = "red", size = 2) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::ggtitle(paste("St.", sta, "- Start = Blue, End = Red, Standard = Grey")) +
    ggplot2::theme_bw()

  ggplot2::ggsave(location, file = paste0("QAQC/Today/Sta", sta, "_HaulPos.png"))
}
