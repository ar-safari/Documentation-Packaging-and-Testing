% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_functions.R
\name{geom_timeline_label}
\alias{geom_timeline_label}
\title{Adds annotations to the earthquake data.}
\usage{
geom_timeline_label(
  mapping = NULL,
  data = NULL,
  stat = "timeline",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  x_min = NULL,
  x_max = NULL,
  n_max = 5
)
}
\arguments{
\item{mapping}{A aesthetics class specifying mapping.}

\item{data}{A data.frame to compute the statistics from.}

\item{stat}{A string specifying a statistics class to use for plotting. Default is
'stat_timeline'.}

\item{position}{A string specifying position of plot.}

\item{...}{Other arguments passed on to layer()}

\item{na.rm}{A boolean value. Remove NA if 'TRUE'.}

\item{show.legend}{A boolean value. Show legend in plot if 'TRUE'.}

\item{inherit.aes}{A boolean value. Inherit aesthetics if 'TRUE'.}

\item{x_min}{A string specifying a date in a 'yyyy-mm-dd' format. Used as a
lowerbound to subset data. Default is NULL. Passed to stat_timeline if used.}

\item{x_max}{A string specifying a date in a 'yyyy-mm-dd' format. Used as a
upperbound to subset data. Default is NULL. Passed to stat_timeline if used.}

\item{n_max}{A numeric. The number of largest earthquakes for which a label
will be added. Default is 5.}
}
\value{
A ggplot2 layer.
}
\description{
\code{geom_timeline_label} adds a vertical line to each data point with a text
annotation (e.g. the location of the earthquake) attached to each line.
There is an option to subset to n_max number of earthquakes, where we take
the n_max largest (by magnitude) earthquakes. Aesthetics are x, which is the
date of the earthquake and label which takes the column name from which
annotations will be obtained.
}
\examples{
\dontrun{
library(ggplot2)
data_raw <- readr::read_delim("./signif.txt")
dat <- eq_clean_data(data_raw) \%>\%
  filter(COUNTRY \%in\% c("MEXICO", "ITALY"))

# plot data on one timeline in uniform size and color
ggplot(dat, aes(x=date)) +
  geom_timeline_label()

# plot data in multiple timelines
ggplot(dat, aes(x=date, y=COUNTRY)) +
  geom_timeline_label()

# plot data in multiple timelines, specifying size by 'INTENSITY', and color by 'TOTAL_DEATHS'
ggplot(dat, aes(x=date, y=COUNTRY, colour=TOTAL_DEATHS, size=INTENSITY)) +
  geom_timeline_label()

# Use parameters 'alpha', 'x_min', 'x_max', and 'n_max'
ggplot(dat, aes(x=date, y=COUNTRY, colour=TOTAL_DEATHS, size=INTENSITY)) +
  geom_timeline_label(alpha=.1, x_min="1980-01-01", x_max="2017-01-01", n_max=10)
}
}
