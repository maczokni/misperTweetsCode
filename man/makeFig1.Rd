\name{makeFig1}
\title{Makefigure 1}
\alias{makeFig1}
\usage{
makeFig1(fig_df)
}
\arguments{
  \item{fig_df}{df needed to make the first chart created with the function makeDemoPhotoPlotDf}
}
\description{
This function creates figure 1
}

\examples{
fig_df <- structure(list(phototype = structure(c(1L, 1L, 2L, 2L, 3L, 3L,
4L), .Label = c("No photo", "Custody photo", "Regular photo",
"Muliple photos"), class = "factor"), gender_coded = c("Female",
"Male", "Female", "Male", "Female", "Male", "Male"), nw_mean = c(NA,
NA, NA, 7, NA, NA, NA), w_mean = c(NA, 0.5, 2, 20, 2, 1, 15),
    nw_median = c(NA, NA, NA, 7, NA, NA, NA), w_median = c(NA,
    0.5, 2, 20, 2, 1, 15), nw_count = c(NA, NA, NA, 1L, NA, NA,
    NA), w_count = c(NA, 2L, 1L, 1L, 1L, 2L, 1L), nw_lqt = c(NA,
    NA, NA, 7, NA, NA, NA), w_lqt = c(NA, 0.25, 2, 20, 2, 0.5,
    15), nw_uqt = c(NA, NA, NA, 7, NA, NA, NA), w_uqt = c(NA,
    0.75, 2, 20, 2, 1.5, 15)), row.names = c(NA, -7L), class = c("grouped_df",
"tbl_df", "tbl", "data.frame"), groups = structure(list(phototype = structure(c(1L,
1L, 2L, 2L, 3L, 3L, 4L), .Label = c("No photo", "Custody photo",
"Regular photo", "Muliple photos"), class = "factor"), gender_coded = c("Female",
"Male", "Female", "Male", "Female", "Male", "Male"), .rows = list(
    1L, 2L, 3L, 4L, 5L, 6L, 7L)), row.names = c(NA, -7L), class = c("tbl_df",
"tbl", "data.frame")))

fig.1 <- makeFig1(fig_df)
}

