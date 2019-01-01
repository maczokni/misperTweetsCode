\name{makeFig2Df}
\title{Make the df needed for figure 2}
\alias{makeFig2Df}
\usage{
makeFig2Df(misper_tweets)
}
\arguments{
  \item{misper_tweets}{cleaned data set of missing persons tweets with all variables coded manually}
}
\description{
This function creates the dataframe needed to be able to make the plot of figure 2 comparing photo quality to retwets broken down by gender and ethnic appearance
}

\examples{
misper_tweets <- structure(list(phototype = structure(c(1L, 1L, 3L, 1L, 1L, 3L,
1L, 2L, 1L, 1L, 2L, 3L, 1L, 1L, 1L, 4L, 1L, 1L, 1L, 2L), .Label = c("No photo",
"Custody photo", "Regular photo", "Muliple photos"), class = "factor"),
    gender_coded = c(NA, NA, "Female", "Male", NA, "Male", "Female",
    "Female", NA, NA, "Male", "Male", "Male", "Male", NA, "Male",
    "Male", "Male", "Male", "Male"), race_coded = c(NA, NA, "white",
    NA, NA, "white", NA, "white", NA, NA, "white", "white", NA,
    NA, NA, "white", "white", "white", NA, "non-white"), retweet_count = c(41L,
    1L, 2L, 85L, 3L, 0L, 0L, 2L, 3L, 12L, 20L, 2L, 1L, 1L, 1L,
    15L, 0L, 1L, 0L, 7L)), row.names = c(NA, 20L), class = "data.frame")

fig_df <- makeFig2Df(misper_tweets)
}

