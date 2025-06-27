#' Make QAQC plots
#'
#' @param depth depths of each skate of gear
#' @param chx check that start and ends are near skate depths
#' @param len3 sablefish length data
#' @param testE even lengths
#' @param testO odd lengths
#' @param StnLoc station locations to compare with standard setting positions
#'
#' @return saved plots
#' @export
#'
#' @examples
make_plots <- function(depth, chx, len3, testE, testO, StnLoc) {
  Deps <- ggplot2::ggplot(data = depth, aes(x = hachi, y = intrpdep, color = factor(haul))) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = chx, aes(x = hachi, y = depth), color = "black", shape = 4, size = 3) +
    ggplot2::scale_y_reverse() +
    ggplot2::xlab("Skate #") +
    ggplot2::ylab("Depth (m)") +
    ggplot2::ggtitle(paste("Station", sta)) +
    ggplot2::theme_bw()

  ggsave(Deps, file=paste("Today/Sta",sta,"_DepthCheck.png"))

  if(length(unique(len3$Haul)) == 2) {
    hist <-  ggplot2::ggplot(data = testE, aes(x = Length, weight = Tot, fill = Sex)) +
      ggplot2::geom_histogram(binwidth = 1) +
      ggplot2::geom_histogram(data = testO, aes(x = Length, weight = Tot, fill = Sex), binwidth=1, alpha=0.5) +
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
                     strip.background = element_blank(),
                     panel.border = theme_L_border(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 12))
  }

  if(length(unique(len3$Haul)) == 1) {
    hist <- ggplot2::ggplot(data = testE, ggplot2::aes(x = Length, weight = Tot, fill = Sex)) +
      ggplot2::geom_histogram(binwidth = 1) +
      ggplot2::geom_histogram(data = testO, aes(x = Length, weight = Tot, fill = Sex), binwidth = 1, alpha = 0.5) +
      ggplot2::geom_hline(yintercept = 0, col = "grey80", size = 0.5) +
      ggplot2::scale_fill_manual(values = c("red", "blue")) +
      ggplot2::xlab("Length (cm)") +
      ggplot2::ylab("Number of Sablefish") +
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::ggtitle(glue("Station {station} - Dark = Even, Light = Odd")) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                     axis.text = ggplot2::element_text(size = 12),
                     strip.background = ggplot2::element_blank(),
                     panel.border = theme_L_border(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 12))
  }

  ggplot2::ggsave(hist, file = paste("Today/Sta", sta, "_LenHist.png"))


  location <- ggplot2::ggplot() +
    ggplot2::geom_point(data = stnd, aes(x = Lon, y = Lat), col = "gray") +
    ggplot2::geom_point(data = StnLoc, aes(x = StartLon, y = StartLat, ), col = "blue", size = 2) +
    ggplot2::geom_point(data = StnLoc, aes(x = EndLon, y = EndLat), col = "red", size = 2) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::ggtitle(paste("St.", sta, "- Start = Blue, End = Red, Standard = Grey")) +
    ggplot2::theme_bw()

  ggplot2::ggsave(location, file = paste("Today/Sta", sta, "_HaulPos.png"))
}




#' Code duplicated from ggplot2 source (not exposed to wider namespace) for convenience
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
len0_null <- function(x) {
  if (length(x) == 0)  NULL
  else                 x
}

#' Title
#'
#' @param type
#' @param colour
#' @param size
#' @param linetype
#'
#' @return
#' @export
#'
#' @examples
theme_border <- function(
    type = c("left", "right", "bottom", "top", "none"),
    colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_border(type=c("bottom","left")) ) + ...
  type <- match.arg(type, several.ok=TRUE)
  structure(
    list(type = type, colour = colour, size = size, linetype = linetype),
    class = c("theme_border", "element_blank", "element")
  )
}



#' Title
#'
#' @param element
#' @param x
#' @param y
#' @param width
#' @param height
#' @param type
#' @param colour
#' @param size
#' @param linetype
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
element_grob.theme_border <- function(
    element, x = 0, y = 0, width = 1, height = 1,
    type = NULL,
    colour = NULL, size = NULL, linetype = NULL,
    ...) {
  if (is.null(type)) type = element$type
  xlist <- c()
  ylist <- c()
  idlist <- c()
  if ("bottom" %in% type) { # bottom
    xlist <- append(xlist, c(x, x+width))
    ylist <- append(ylist, c(y, y))
    idlist <- append(idlist, c(1,1))
  }
  if ("top" %in% type) { # top
    xlist <- append(xlist, c(x, x+width))
    ylist <- append(ylist, c(y+height, y+height))
    idlist <- append(idlist, c(2,2))
  }
  if ("left" %in% type) { # left
    xlist <- append(xlist, c(x, x))
    ylist <- append(ylist, c(y, y+height))
    idlist <- append(idlist, c(3,3))
  }
  if ("right" %in% type) { # right
    xlist <- append(xlist, c(x+width, x+width))
    ylist <- append(ylist, c(y, y+height))
    idlist <- append(idlist, c(4,4))
  }
  if (length(type)==0 || "none" %in% type) { # blank; cannot pass absence of coordinates, so pass a single point and use an invisible line
    xlist <- c(x,x)
    ylist <- c(y,y)
    idlist <- c(5,5)
    linetype <- "blank"
  }
  gp <- grid::gpar(lwd = len0_null(size * 2.834646),
                   col = colour,
                   lty = linetype)
  element_gp <- grid::gpar(lwd = len0_null(element$size * 2.834646),
                           col = element$colour,
                           lty = element$linetype)
  grid::polylineGrob(
    x = xlist, y = ylist, id = idlist, ..., default.units = "npc",
    gp = utils::modifyList(element_gp, gp),
  )
}




#' For convenience: "L" (left + bottom) border
#'
#' @param colour
#' @param size
#' @param linetype
#'
#' @return
#' @export
#'
#' @examples
theme_L_border <- function(colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + theme( panel.border=theme_L_border() ) + ...
  structure(
    list(colour = colour, size = size, linetype = linetype),
    class = c("theme_L_border", "element_blank", "element")
  )
}

#' Title
#'
#' @param element
#' @param x
#' @param y
#' @param width
#' @param height
#' @param colour
#' @param size
#' @param linetype
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
element_grob.theme_L_border <- function(
  element, x = 0, y = 0, width = 1, height = 1,
    colour = NULL, size = NULL, linetype = NULL,
    ...) {
  gp <- grid::gpar(lwd = len0_null(size * 2.834646),
                   col = colour,
                   lty = linetype)
  element_gp <- grid::gpar(lwd = len0_null(element$size * 2.834646),
                           col = element$colour,
                           lty = element$linetype)
  grid::polylineGrob(
    x = c(x+width, x, x), y = c(y,y,y+height), ..., default.units = "npc",
    gp = utils::modifyList(element_gp, gp),
  )
}
