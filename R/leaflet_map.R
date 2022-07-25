# week 3 functions: mapping tools

#' Creates an HTML label for a data point.
#'
#' `eq_create_label()` takes the dataset as an argument and creates an HTML
#' label that can be used as the annotation text in the leaflet map. This
#' function puts together a character string for each earthquake that will show
#' the cleaned location (cleaned LOCATION_NAME), the magnitude (EQ_PRIMARY),
#' and the total number of deaths (TOTAL_DEATHS), with boldface labels for
#' each ("Location", "Total deaths", and "Magnitude"). If an earthquake is
#' missing values for any of these, both the label and the value will be
#' skipped for that element of the tag.
#'
#' @param df A data.frame for which a list of labels will be generated. It must
#' have columns named 'LOCATION_NAME', 'EQ_PRIMARY', and 'TOTAL_DEATHS'.
#' @returns A list of characters, specifying the label for each data point.
#'
#' @examples
#' \dontrun{
#' data_raw <- readr::read_delim("./signif.txt")
#' dat <- eq_clean_data(data_raw) %>%
#'   filter(COUNTRY %in% c("MEXICO", "ITALY"))
#'
#' label_1 <- eq_create_label(dat[301,])
#' # "<b>Location: </b>Guerrero<br><b>Magnitude: </b> 8.3<br><b>Total deaths: </b>8<br>"
#' }
#' @export
eq_create_label <- function(df){

  label <- apply(df, 1, function(rr){
    ret <- ifelse(is.na(rr[["LOCATION_NAME"]]), "",
                  paste0("<b>Location: </b>", rr[["LOCATION_NAME"]],"<br>"))
    ret <- paste0(ret, ifelse(is.na(rr[["EQ_PRIMARY"]]), "",
                              paste0("<b>Magnitude: </b>", rr[["EQ_PRIMARY"]],"<br>")))
    ret <- paste0(ret, ifelse(is.na(rr[["TOTAL_DEATHS"]]), "",
                              paste0("<b>Total deaths: </b>", rr[["TOTAL_DEATHS"]],"<br>")))
  })

  return(label)
}

#' Visualizes earthquakes.
#'
#' `eq_map()` takes an argument data containing the filtered data frame with
#' earthquakes to visualize. The function maps the epicenters (LATITUDE/LONGITUDE)
#' and annotates each point with in pop up window containing annotation data
#' stored in a column of the data frame. The user can choose which column is
#' used for the annotation in the pop-up with a function argument named
#' annot_col. Each earthquake should be shown with a circle, and the radius
#' of the circle should be proportional to the earthquake's magnitude (EQ_PRIMARY).
#'
#' @param df A data.frame. The input earthquake data.
#' @param annot_col A string, specifying name of the column to be used for annotation
#' in the pop-up text. If "popup_text" is chosen, the annotation label will be a
#' complex label created by `eq_create_label()`. Default is "date".
#' @returns A leaflet. Contains an interactive map of earthquakes and annotations.
#'
#' @importFrom dplyr mutate
#' @importFrom leaflet leaflet addTiles addCircles
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # create a leaflet map using 'date' as labels
#' readr::read_delim("./signif.txt") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   eq_map(annot_col = "date")
#'
#' # create a leaflet map using complex label
#' readr::read_delim("./signif.txt") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#' @export
eq_map <- function(df, annot_col = "date"){

  df <- df %>%
    dplyr::mutate(EQ_PRIMARY = as.numeric(EQ_PRIMARY))

  lf <- leaflet::leaflet(df) %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(
      lng = ~LONGITUDE,
      lat = ~LATITUDE,
      weight = 1,
      radius = ~EQ_PRIMARY * 10000,
      popup = ~eval(parse(text = annot_col))
    )

  return(lf)
}



