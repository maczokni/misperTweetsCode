\name{makeHashDf}
\title{Make the df needed for hashtag type stats}
\alias{makeHashDf}
\usage{
makeHashDf(misper_tweets)
}
\arguments{
  \item{misper_tweets}{cleaned data set of missing persons tweets with all variables coded manually}
}
\description{
This function creates the dataframe needed to be able to look at the different hashtag types
}

\examples{
misper_tweets <- structure(list(retweet_count = c(15L, 7L, 6L, 9L, 24L, 15L, 27L,
21L, 4L, 1L, 52L, 2L, 18L, 1L, 1L, 5L, 6L, 10L, 3L, 2L), coded_hash_type = c("other",
"police", "other", "location", "location", "missing", "location",
"missing", "missing", "missing", "location", "location", "other",
"missing", "missing", "missing", "missing", "missing", "missing",
"missing")), row.names = c(NA, 20L), class = "data.frame")



hash_df <- makeHashDf(misper_tweets)
}

