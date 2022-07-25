## test functions
# read data
data_raw <- readr::read_delim("signif.txt", show_col_types = FALSE)

# test clean_up.R functions
test_that("eq_location_clean removes string before colon", {
  test_str <- "Removed: Retained"
  expect_equal(eq_location_clean(test_str), "Retained")
})

test_that("eq_clean_data generates correct columns", {
  df1 <- eq_clean_data(data_raw)
  expect_true("date" %in% names(df1))
  expect_type(df1$LONGITUDE, "double")
  expect_type(df1$LATITUDE, "double")
})

# test geom_function.R functions
df1 <- eq_clean_data(data_raw)
df2 <- df1 %>%
  dplyr::filter(COUNTRY %in% c("ITALY", "MEXICO"))

test_that("stat_timeline can be used with geom_point", {
  p0 <- ggplot2::ggplot(df2, ggplot2::aes(x=date)) +
    stat_timeline(geom="point")
  expect_s3_class(p0$layers[[1]]$geom, "GeomPoint")
})

test_that("stat_timeline can subset data and use parameter", {
  p1 <- ggplot2::ggplot(df2, ggplot2::aes(x=date, y=COUNTRY,
                                          size=INTENSITY, colour=TOTAL_DEATHS)) +
    stat_timeline(alpha=.3, geom="point", x_min="1980-01-01", x_max="2017-01-01")
  expect_error(p1, NA)
})

test_that("stat_timeline can deal with absent 'y' aesthetics correctly", {
  p2 <- ggplot2::ggplot(df2, ggplot2::aes(x=date, size=INTENSITY)) +
    stat_timeline(alpha=.3, geom="point", x_min="1980-01-01", x_max="2017-01-01")
  expect_error(p2, NA)
})

test_that("geom_timeline is correctly implemented", {
  p3 <- ggplot2::ggplot(df2, ggplot2::aes(x=date, y=COUNTRY,
                                          colour=TOTAL_DEATHS, size=INTENSITY)) +
    geom_timeline(alpha=.1, x_min="1980-01-01", x_max="2017-01-01")

  expect_error(p3, NA)
  expect_s3_class(p3$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p3$layers[[1]]$stat, "StatTimeline")
})

test_that("theme_timeline is correctly implemented", {
  p4 <- ggplot2::ggplot(df2, ggplot2::aes(x=date, y=COUNTRY,
                                          colour=TOTAL_DEATHS, size=INTENSITY)) +
    geom_timeline(alpha=.1, x_min="1980-01-01", x_max="2017-01-01") +
    theme_timeline() +
    ggplot2::xlab("DATE") +
    ggplot2::guides(size = ggplot2::guide_legend(title = "Richter scale value", order = 1),
                    colour = ggplot2::guide_colorbar(title = "# deaths", order = 2))

  expect_error(p4, NA)
})

test_that("geom_timeline_label is correctly implemented", {
  p5 <- ggplot2::ggplot(df2, ggplot2::aes(x=date, y=COUNTRY, colour=TOTAL_DEATHS, size=INTENSITY,
                                          label=LOCATION_NAME)) +
    geom_timeline_label(alpha=.1, x_min="1980-01-01", x_max="2017-01-01",
                        n_max=10) +
    theme_timeline() +
    ggplot2::xlab("DATE") +
    ggplot2::guides(size = ggplot2::guide_legend(title = "Richter scale value", order = 1),
                    colour = ggplot2::guide_colorbar(title = "# deaths", order = 2)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(.5, 1.5)))

  expect_error(p5, NA)
})

# test leaflet_map.R functions
test_that("test `eq_map` is correctly implemented ", {
  lf <- readr::read_delim("./signif.txt", show_col_types = FALSE) %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
    eq_map(annot_col = "date")

  expect_error(lf, NA)
})

test_that("test `eq_create_label` is correctly implemented", {
  lf <- readr::read_delim("./signif.txt", show_col_types = FALSE) %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")

  expect_error(lf, NA)
})
