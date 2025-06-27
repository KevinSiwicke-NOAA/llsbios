#' Query the at-sea Access database for catch data
#'
#' @param channel channel to the at-sea database
#' @param sta_num station number
#'
#' @return list of data from database
#' @export
#'
#' @examples
#' \dontrun{
#' get_data(channel, sta_num = 70)
#' }
get_data <- function(channel, sta_num) {
  depth <- RODBC::sqlQuery(channel, base::paste0("select * from Depth where station = ", sta_num)) %>%
    dplyr::rename_all(tolower)

  position <- RODBC::sqlQuery(channel, base::paste0("select * from Haul_position where station = ", sta_num)) %>%
    dplyr::rename_all(tolower)

  length <- RODBC::sqlQuery( channel, paste ("select * from Length where species_code = 20510 and station = ", sta_num)) %>%
    dplyr::rename_all(tolower)

  RODBC::odbcClose(channel)

  stnds <- readRDS(system.file("extdata", "std_set.rds", package = "llsbios"))
  stnd <- stnds[stnds$Station == sta_num, ]

  dat_list <- list(depth, position, length, stnd)
}


#' Prepare data for plots
#'
#' @param depth depth by skate
#' @param position position by station
#' @param length lengths
#'
#' @return list of data used for making plots
#' @export
#'
#' @examples
make_plot_data <- function(depth, position, length) {
  # Depth checks
  # For stations with two hauls, separate data
  haulAM <- base::min(depth$haul)

  Zx1 <- base::as.data.frame(c(min(depth$hachi) - 0.5, max(depth$hachi) + 0.5, min(depth$hachi) + 60))
  Zy1 <- base::as.data.frame(t(position[position$haul == haulAM, c(25, 26, 29)]))
  one <- base::cbind(Zx1, Zy1)
  base::names(one) <- c("hachi", "depth")

  chx <- one

  if(length(unique(depth$haul)) > 1) {
    haulPM <- base::min(depth$haul) + 1

    Zx1 <- base::as.data.frame(c(min(depth$hachi) - 0.5, max(depth$hachi) / 2 + 0.25, min(depth$hachi) + 60))
    Zy1 <- base::as.data.frame(t(position[position$haul == haulAM, c(25, 26, 29)]))
    one <- base::cbind(Zx1, Zy1)
    names(one) <- c("hachi", "depth")

    chx <- one
    Zx2 <- as.data.frame(c(max(depth$hachi)/2+0.75, max(depth$hachi) + 0.5, max(depth$hachi) / 2 + 60))
    Zy2 <- as.data.frame(t(position[position$haul == haulPM, c(25, 26, 29)]))
    two <- cbind(Zx2, Zy2)
    names(two) <- c("hachi", "depth")

    chx <- rbind(one, two)
  }

  len <- length[,c(4,3,5,6,7,8,9)]
  names(len) <- c("Station", "Haul", "SpeciesCode", "Sex", "DepthStratum", "Length", "Num")

  lengths <- len %>%
    dplyr::group_by(Station, Haul, Sex, Length) %>%
    dplyr::summarize(Tot = sum(Num))

  lengths$lab <- ifelse((lengths$Length %% 2) == 0,"Even","Odd")

  lengths$Tot <- ifelse(lengths$Sex==2, -lengths$Tot, lengths$Tot)

  lengths$Sex = ifelse(lengths$Sex==1, "Male",
                    ifelse(lengths$Sex == 2, "Female", "Unsexed"))

  lenE <- lengths[lengths$lab == "Even", ]
  lenO <- lengths[lengths$lab == "Odd", ]

  ###Check start/end points relative to expected locations of longline
  ###
  StnLoc <- position[, c(3, 4, 7, 10, 13, 16)]
  names(StnLoc) <- c("Station", "Haul", "StartLat", "StartLon", "EndLat", "EndLon")

  #Convert screwy lat/long format
  StartLat <- StnLoc %>%
    dplyr::separate(StartLat, c("deg", "min"), 2, convert = TRUE)
  StnLoc$StartLat <- StartLat$deg + StartLat$min / 60
  StartLon <- StnLoc %>%
    dplyr::separate(StartLon, c("deg", "min"), 3, convert = TRUE)
  StnLoc$StartLon <- -StartLon$deg - StartLon$min / 60
  EndLat <- StnLoc %>%
    dplyr::separate(EndLat, c("deg", "min"), 2, convert = TRUE)
  StnLoc$EndLat <- EndLat$deg + EndLat$min / 60
  EndLon <- StnLoc %>%
    dplyr::separate(EndLon, c("deg", "min"), 3, convert = TRUE)
  StnLoc$EndLon <- -EndLon$deg - EndLon$min / 60

  plot_data <- list(depth, chx, lengths, lenE, lenO, StnLoc)
}
