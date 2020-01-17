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
misper_tweets <- structure(list(retweet_count = c(41L, 1L, 2L, 85L, 3L, 0L, 0L,
2L, 3L, 12L, 20L, 2L, 1L, 1L, 1L, 15L, 0L, 1L, 0L, 7L, 2L, 10L,
6L, 1L, 1L, 6L, 2L, 6L, 13L, 4L, 0L, 8L, 4L, 5L, 5L, 0L, 15L,
79L, 9L, 1L, 9L, 24L, 28L, 15L, 5L, 27L, 2L, 22L, 20L, 7L), image_quality_coded = c(NA,
NA, "good/excellent", NA, NA, "bad", NA, "average", NA, NA, "average",
"average", NA, NA, NA, "bad", NA, NA, NA, "bad", "average", NA,
"average", NA, NA, "average", NA, "average", "bad", NA, NA, NA,
NA, NA, NA, "average", NA, "bad", "average", "average", "average",
"good/excellent", "good/excellent", "average", "average", "average",
NA, "good/excellent", NA, "good/excellent"), gender_coded = c(NA,
NA, "female", "male", NA, "male", "female", "female", NA, NA,
"male", "male", "male", "male", NA, "male", "male", "male", "male",
"male", "female", "female", "male", "female", "female", "female",
"female", "male", NA, NA, NA, "male", NA, "female", NA, "male",
NA, "male", "male", "male", "male", "male", "male", "male", "male",
"male", NA, "male", NA, "male"), race_coded = c(NA, NA, "white",
NA, NA, "white", NA, NA, NA, NA, "white", "white", NA, NA,
NA, "white", "white", "white", NA, "non-white", "white", NA,
"white", NA, NA, "non-white", NA, "white", "white", NA, NA, "white",
NA, "white", NA, "white", NA, "white", "white", "white", "white",
"white", "white", "white", "non-white", "white", NA, "white",
NA, "white")), row.names = c(NA, 50L), class = "data.frame")

fig_df <- makeFig2Df(misper_tweets)
}

