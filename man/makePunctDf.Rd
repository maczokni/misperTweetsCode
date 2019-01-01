\name{makePunctDf}
\title{Make the df needed for punctuation stats}
\alias{makePunctDf}
\usage{
makePunctDf(misper_tweets)
}
\arguments{
  \item{misper_tweets}{cleaned data set of missing persons tweets with all variables coded manually}
}
\description{
This function creates the dataframe needed to be able to look at the different punctuation uses
}

\examples{
misper_tweets <- structure(list(retweet_count = c(41L, 1L, 2L, 85L, 3L, 0L, 0L,
2L, 3L, 12L), ast_yn = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L,
1L, 1L, 1L), .Label = c("0", "1"), class = "factor"), qm_yn = structure(c(1L,
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L), .Label = c("0", "1"), class = "factor"),
    exc_yn = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
    1L), .Label = c("0", "1"), class = "factor"), hasht_yn = structure(c(2L,
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("0", "1"), class = "factor")), row.names = c(NA, 10L), class = "data.frame")

punct_df <- makePunctDf(misper_tweets)
}

