% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_functions.R
\name{stat_timeline}
\alias{stat_timeline}
\title{Prepare data for geom_timeline or geom_timeline_label.}
\usage{
stat_timeline(
  mapping = NULL,
  data = NULL,
  geom = "timeline",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  x_min = NULL,
  x_max = NULL,
  ...
)
}
\arguments{
\item{mapping}{A aesthetics class specifying mapping.}

\item{data}{A data.frame to compute the statistics from.}

\item{geom}{A string specifying a geom class to use for plotting. Default is
'geom_timeline'.}

\item{position}{A string specifying position of plot.}

\item{show.legend}{A boolean value. Show legend in plot if 'TRUE'}

\item{inherit.aes}{A boolean value. Inherit aesthetics if 'TRUE'}

\item{x_min}{A string specifying a date in a 'yyyy-mm-dd' format. Used as a
lowerbound to subset data. Default is NULL.}

\item{x_max}{A string specifying a date in a 'yyyy-mm-dd' format. Used as a
upperbound to subset data. Default is NULL.}

\item{...}{Other arguments passed on to layer()}
}
\value{
A ggplot2 layer.
}
\description{
For the input data, if the 'y' column is missing, it adds a 'y' column (all
data points in one group). If the 'x_min' and/or 'x_max' parameter is supplied,
it subsets the input data to the range of time specified by 'x_min' and/or
'x_max'.
}
\examples{
\dontrun{
library(ggplot2)
# read data
data_raw <- readr::read_delim("./signif.txt")
dat <- eq_clean_data(data_raw) \%>\%
  filter(COUNTRY \%in\% c("MEXICO", "ITALY"))

# plots all data points in uniform color and size:
ggplot(dat, aes(x=date)) +
  stat_timeline(geom="point")

# plots a subset of data points specified by 'x_min' and 'x_max':
ggplot(dat, aes(x=date)) +
  stat_timeline(geom="point", x_min="1980-01-01", x_max="2017-01-01")

# specifies size of points by 'INTENSITY', and color by 'TOTAL_DEATHS':
ggplot(dat, aes(x=date, size=INTENSITY, colour=TOTAL_DEATHS)) +
  stat_timeline(geom="point", x_min="1980-01-01", x_max="2017-01-01")

# plots multiple countries by specifying 'y':
ggplot(dat, aes(x=date, y=COUNTRY, size=INTENSITY, colour=TOTAL_DEATHS)) +
  stat_timeline(geom="point", x_min="1980-01-01", x_max="2017-01-01")
}

}
