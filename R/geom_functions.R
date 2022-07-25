# week 2 functions: build a geom

## stats ----------------------------------------------------------------------

#' Prepare data for geom_timeline or geom_timeline_label.
#'
#' For the input data, if the 'y' column is missing, it adds a 'y' column (all
#' data points in one group). If the 'x_min' and/or 'x_max' parameter is supplied,
#' it subsets the input data to the range of time specified by 'x_min' and/or
#' 'x_max'.
#'
#' @param mapping A aesthetics class specifying mapping.
#' @param data A data.frame to compute the statistics from.
#' @param geom A string specifying a geom class to use for plotting. Default is
#' 'geom_timeline'.
#' @param position A string specifying position of plot.
#' @param show.legend A boolean value. Show legend in plot if 'TRUE'
#' @param inherit.aes A boolean value. Inherit aesthetics if 'TRUE'
#' @param x_min A string specifying a date in a 'yyyy-mm-dd' format. Used as a
#' lowerbound to subset data. Default is NULL.
#' @param x_max A string specifying a date in a 'yyyy-mm-dd' format. Used as a
#' upperbound to subset data. Default is NULL.
#' @param ... Other arguments passed on to layer()
#' @returns A ggplot2 layer.
#'
#' @importFrom ggplot2 layer ggproto Stat
#' @importFrom lubridate ymd
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # read data
#' data_raw <- readr::read_delim("./signif.txt")
#' dat <- eq_clean_data(data_raw) %>%
#'   filter(COUNTRY %in% c("MEXICO", "ITALY"))
#'
#' # plots all data points in uniform color and size:
#' ggplot(dat, aes(x=date)) +
#'   stat_timeline(geom="point")
#'
#' # plots a subset of data points specified by 'x_min' and 'x_max':
#' ggplot(dat, aes(x=date)) +
#'   stat_timeline(geom="point", x_min="1980-01-01", x_max="2017-01-01")
#'
#' # specifies size of points by 'INTENSITY', and color by 'TOTAL_DEATHS':
#' ggplot(dat, aes(x=date, size=INTENSITY, colour=TOTAL_DEATHS)) +
#'   stat_timeline(geom="point", x_min="1980-01-01", x_max="2017-01-01")
#'
#' # plots multiple countries by specifying 'y':
#' ggplot(dat, aes(x=date, y=COUNTRY, size=INTENSITY, colour=TOTAL_DEATHS)) +
#'   stat_timeline(geom="point", x_min="1980-01-01", x_max="2017-01-01")
#' }
#'
#' @export
stat_timeline <- function(mapping = NULL, data = NULL,
                          geom = "timeline", position = "identity",
                          show.legend = NA,
                          inherit.aes = TRUE,
                          x_min = NULL,
                          x_max = NULL, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatTimeline,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      x_min = x_min,
      x_max = x_max,
      ...
    )
  )
}

#' stat_timeline constructor.
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @details extends Stat
#' @export
StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,
                                 required_aes = c("x"),

                                 setup_data = function(data, params){
                                   if(!("y" %in% names(data))){
                                     data$y <- 1
                                   }
                                   data
                                 },

                                 compute_panel = function(data, scales, x_min, x_max){
                                   # subset data
                                   if(!is.null(x_min)){
                                     data <- data[data$x >=  as.numeric(lubridate::ymd(x_min)),,drop=FALSE]
                                   }
                                   if(!is.null(x_max)){
                                     data <- data[data$x <=  as.numeric(lubridate::ymd(x_max)),,drop=FALSE]
                                   }
                                   data
                                 }
)

## geom -----------------------------------------------------------------------
#' The timeline geom plots a time line of earthquakes.
#'
#' `geom_timeline()` subsets the input data to a range specified by 'xmin' and/or
#' 'xmax', if these parameters are supplied. Then it plots time line on which each
#' point is an earthquake. Optional aesthetics include color, size, and alpha
#' (for transparency). The xaesthetic is a date and an optional y aesthetic is
#' a factor indicating some stratification in which case multiple time lines
#' will be plotted for each level of the factor (e.g. country).
#'
#' @param mapping A aesthetics class specifying mapping.
#' @param data A data.frame to compute the statistics from.
#' @param stat A string specifying a statistics class to use for plotting. Default is
#' 'stat_timeline'.
#' @param position A string specifying position of plot.
#' @param show.legend A boolean value. Show legend in plot if 'TRUE'.
#' @param na.rm A boolean value. Remove NA if 'TRUE'.
#' @param inherit.aes A boolean value. Inherit aesthetics if 'TRUE'.
#' @param x_min A string specifying a date in a 'yyyy-mm-dd' format. Used as a
#' lowerbound to subset data. Default is NULL. Passed to stat_timeline if used.
#' @param x_max A string specifying a date in a 'yyyy-mm-dd' format. Used as a
#' upperbound to subset data. Default is NULL. Passed to stat_timeline if used.
#' @param ... Other arguments passed on to layer()
#' @returns A ggplot2 layer.
#'
#' @importFrom ggplot2 layer ggproto GeomPoint aes
#' @importFrom grid unit segmentsGrob gpar grobTree
#' @importFrom dplyr group_by summarize
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # read data
#' data_raw <- readr::read_delim("./signif.txt")
#' dat <- eq_clean_data(data_raw) %>%
#'   filter(COUNTRY %in% c("MEXICO", "ITALY"))
#'
#' # plot data on one timeline in uniform size and color
#' ggplot(dat, aes(x=date)) +
#'   geom_timeline()
#'
#' # plot data in multiple timelines
#' ggplot(dat, aes(x=date, y=COUNTRY)) +
#'   geom_timeline()
#'
#' # plot data in multiple timelines, specifying size by 'INTENSITY', and color by 'TOTAL_DEATHS'
#' ggplot(dat, aes(x=date, y=COUNTRY, colour=TOTAL_DEATHS, size=INTENSITY)) +
#'   geom_timeline()
#'
#' # Use parameters 'alpha', 'x_min', and 'x_max'
#' ggplot(dat, aes(x=date, y=COUNTRY, colour=TOTAL_DEATHS, size=INTENSITY)) +
#'   geom_timeline(alpha=.1, x_min="1980-01-01", x_max="2017-01-01")
#' }
#' @export
geom_timeline <- function(mapping = NULL, data = NULL,
                       stat = "timeline", position = "identity",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       x_min = NULL,
                       x_max = NULL, ...) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      x_min = x_min,
      x_max = x_max,
      ...
    )
  )
}

#' geom_timeline constructor.
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @details extends GeomPoint
#' @export
GeomTimeline <- ggplot2::ggproto("GeomPoint", ggplot2::GeomPoint,
                                 required_aes = c("x"),
                                 non_missing_aes = c("y", "size", "colour"),

                                 default_aes = ggplot2::aes(
                                   shape = 19, colour = 1, size = 1.5, fill = NA,
                                   alpha = .3, stroke = 0.5
                                 ),

                                 setup_data = function(data, params){

                                   # remove na values in the required_aes
                                   data <- data[!is.na(data$x),,drop=FALSE]

                                   # remove na values in the non_missing_aes
                                   if("colour" %in% names(data)){
                                     data <- data[!is.na(data$colour),,drop=FALSE]
                                   }

                                   if("size" %in% names(data)){
                                     data <- data[!is.na(data$size),,drop=FALSE]
                                   }
                                   data
                                 },

                                 # default_aes is applied after setup_data, and before draw_*
                                 draw_panel = function(data, panel_params, coord){

                                   point_grob <- ggplot2::GeomPoint$draw_panel(
                                     data = data,
                                     panel_params = panel_params,
                                     coord = coord
                                   )
                                   #
                                   # data for the reference line
                                   coords <- coord$transform(data, panel_params)
                                   data2 <- coords %>%
                                     dplyr::group_by(y) %>%
                                     dplyr::summarize(x1 = min(x),
                                               x2 = max(x))

                                   segment_data <- data.frame(
                                     x = min(data2$x1),
                                     xend = max(data2$x2),
                                     y = data2$y,
                                     yend = data2$y
                                   )

                                   segment_grob <- grid::segmentsGrob(
                                     x0 = grid::unit(segment_data$x, "npc"),
                                     y0 = grid::unit(segment_data$y, "npc"),
                                     x1 = grid::unit(segment_data$xend, "npc"),
                                     y1 = grid::unit(segment_data$yend, "npc"),
                                     gp = grid::gpar(col = "black", alpha = .3, lwd = 3)
                                   )


                                   grid::grobTree(point_grob,
                                                  segment_grob)

                                 }

)

## --------------------------------------------------------------------------
#' Adds annotations to the earthquake data.
#'
#' `geom_timeline_label` adds a vertical line to each data point with a text
#' annotation (e.g. the location of the earthquake) attached to each line.
#' There is an option to subset to n_max number of earthquakes, where we take
#' the n_max largest (by magnitude) earthquakes. Aesthetics are x, which is the
#' date of the earthquake and label which takes the column name from which
#' annotations will be obtained.
#'
#' @param mapping A aesthetics class specifying mapping.
#' @param data A data.frame to compute the statistics from.
#' @param stat A string specifying a statistics class to use for plotting. Default is
#' 'stat_timeline'.
#' @param position A string specifying position of plot.
#' @param show.legend A boolean value. Show legend in plot if 'TRUE'.
#' @param na.rm A boolean value. Remove NA if 'TRUE'.
#' @param inherit.aes A boolean value. Inherit aesthetics if 'TRUE'.
#' @param x_min A string specifying a date in a 'yyyy-mm-dd' format. Used as a
#' lowerbound to subset data. Default is NULL. Passed to stat_timeline if used.
#' @param x_max A string specifying a date in a 'yyyy-mm-dd' format. Used as a
#' upperbound to subset data. Default is NULL. Passed to stat_timeline if used.
#' @param n_max A numeric. The number of largest earthquakes for which a label
#' will be added. Default is 5.
#' @param ... Other arguments passed on to layer()
#' @returns A ggplot2 layer.
#'
#' @importFrom ggplot2 layer ggproto GeomPoint aes
#' @importFrom grid unit segmentsGrob textGrob gpar grobTree
#' @importFrom dplyr arrange group_by summarize slice
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data_raw <- readr::read_delim("./signif.txt")
#' dat <- eq_clean_data(data_raw) %>%
#'   filter(COUNTRY %in% c("MEXICO", "ITALY"))
#'
#' # plot data on one timeline in uniform size and color
#' ggplot(dat, aes(x=date)) +
#'   geom_timeline_label()
#'
#' # plot data in multiple timelines
#' ggplot(dat, aes(x=date, y=COUNTRY)) +
#'   geom_timeline_label()
#'
#' # plot data in multiple timelines, specifying size by 'INTENSITY', and color by 'TOTAL_DEATHS'
#' ggplot(dat, aes(x=date, y=COUNTRY, colour=TOTAL_DEATHS, size=INTENSITY)) +
#'   geom_timeline_label()
#'
#' # Use parameters 'alpha', 'x_min', 'x_max', and 'n_max'
#' ggplot(dat, aes(x=date, y=COUNTRY, colour=TOTAL_DEATHS, size=INTENSITY)) +
#'   geom_timeline_label(alpha=.1, x_min="1980-01-01", x_max="2017-01-01", n_max=10)
#' }
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "timeline", position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                x_min = NULL,
                                x_max = NULL,
                                n_max = 5) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      x_min = x_min,
      x_max = x_max,
      n_max = n_max,
      ...
    )
  )
}


#' geom_timeline_label constructor.
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @details extends GeomPoints
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomPoint", ggplot2::GeomPoint,
                                      required_aes = c("x", "label"),
                                      non_missing_aes = c("y", "size", "colour"),

                                      default_aes = ggplot2::aes(
                                        shape = 19, colour = 1, size = 1.5, fill = NA,
                                        alpha = .3, stroke = 0.5
                                      ),

                                      setup_data = function(data, params){

                                        # remove na values in the required_aes
                                        data <- data[!is.na(data$x),,drop=FALSE]

                                        # remove na values in the non_missing_aes
                                        if("colour" %in% names(data)){
                                          data <- data[!is.na(data$colour),,drop=FALSE]
                                        }

                                        if("size" %in% names(data)){
                                          data <- data[!is.na(data$size),,drop=FALSE]
                                        }
                                        data
                                      },

                                      # default_aes is applied after setup_data, and before draw_*
                                      draw_panel = function(data, panel_params, coord, n_max){

                                        point_grob <- ggplot2::GeomPoint$draw_panel(
                                          data = data,
                                          panel_params = panel_params,
                                          coord = coord
                                        )
                                        #
                                        # data for the reference line
                                        coords <- coord$transform(data, panel_params)
                                        data2 <- coords %>%
                                          dplyr::group_by(y) %>%
                                          dplyr::summarize(x1 = min(x),
                                                           x2 = max(x))

                                        segment_data <- data.frame(
                                          x = min(data2$x1),
                                          xend = max(data2$x2),
                                          y = data2$y,
                                          yend = data2$y
                                        )

                                        segment_grob <- grid::segmentsGrob(
                                          x0 = grid::unit(segment_data$x, "npc"),
                                          y0 = grid::unit(segment_data$y, "npc"),
                                          x1 = grid::unit(segment_data$xend, "npc"),
                                          y1 = grid::unit(segment_data$yend, "npc"),
                                          gp = grid::gpar(col = "black", alpha = .3, lwd = 3)
                                        )

                                        # annotation of the top five data points
                                        data3 <- coords %>%
                                          dplyr::arrange(-size) %>%
                                          dplyr::group_by(y) %>%
                                          dplyr::slice(1:n_max)

                                        # vertical lines
                                        annotate_grob <- grid::segmentsGrob(
                                          x0 = grid::unit(data3$x, "npc"),
                                          y0 = grid::unit(data3$y, "npc"),
                                          x1 = grid::unit(data3$x, "npc"),
                                          y1 = grid::unit(data3$y + .1, "npc"),
                                          gp = grid::gpar(col = "black", alpha = .3, lwd = 1)
                                        )

                                        # annotation text
                                        text_grob <- grid::textGrob(
                                          label = data3$label,
                                          x = grid::unit(data3$x, "npc"),
                                          y = grid::unit(data3$y + .1, "npc"),
                                          just = "left",
                                          rot = 45,
                                          check.overlap = TRUE,
                                          gp = grid::gpar(col = "black",
                                                          fontsize = 8)
                                        )


                                        grid::grobTree(point_grob,
                                                       segment_grob,
                                                       annotate_grob,
                                                       text_grob)

                                      }

)

## theme ----------------------------------------------------------------------

#' The timeline theme for 'geom_timeline' or 'geom_timeline_label'.
#'
#' `theme_timeline()` adds theme specifications to a ggplot2 layer created by
#' 'geom_timeline' or 'geom_timeline_label'.
#'
#' @importFrom ggplot2 theme element_line element_text element_blank
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data_raw <- readr::read_delim("./signif.txt")
#' dat <- eq_clean_data(data_raw) %>%
#'   filter(COUNTRY %in% c("MEXICO", "ITALY"))
#'
#' # add theme
#' ggplot(dat, aes(x=date, y=COUNTYR, colour=TOTAL_DEATHS, size=INTENSITY)) +
#'   geom_timeline(alpha=.1, x_min="1980-01-01", x_max="2017-01-01") +
#'   theme_timeline() +
#'   xlab("DATE") +
#'   guides(size = guide_legend(title = "Richter scale value", order = 1),
#'     colour = guide_colorbar(title = "# deaths", order = 2))
#' }
#' @export
theme_timeline <- function(){
  ggplot2::theme(
    axis.line.x  = ggplot2::element_line(colour = "black",
                                         size = .8,
                                         linetype = "solid"),
    axis.ticks.x.bottom = ggplot2::element_line(colour = "black",
                                                size = .8,
                                                linetype = "solid"),
    axis.text.x = ggplot2::element_text(size = 10),

    axis.text.y.left = ggplot2::element_text(size = 10),
    legend.position = "bottom",
    axis.ticks.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.title.y.left = ggplot2::element_blank()
  )
}
