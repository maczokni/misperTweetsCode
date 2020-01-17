\name{makeRegressionPlots}
\title{makeRegressionPlots}
\alias{makeRegressionPlots}
\usage{
makeRegressionPlots(modeltoplot)
}
\arguments{
  \item{modeltoplot}{a regression model object to plot results from}
}
\description{
This function creates a regression plot
}

\examples{

misper_tweets <- structure(list(retweet_count = c(41L, 1L, 2L, 85L, 3L, 0L, 0L,
2L, 3L, 12L), post_length = c(107L, 95L, 147L, 138L, 129L, 100L,
117L, 138L, 114L, 139L), useful_information_yn = c("Y", "Y",
"N", "N", "N", "N", "Y", "N", "N", "N"), diffdate = c(2061, 598,
1044, 464, 2061, 2014, 2087, 2015, 1954, 1942)), row.names = c(NA,
10L), class = "data.frame")

po_combo_model_w_offset <- glm(formula = retweet_count ~
                                 post_length +
                                 useful_information_yn +
                                 offset(log(diffdate)),
                               data = misper_tweets[misper_tweets$diffdate != 0,], family="poisson")

makeRegressionPlots(po_combo_model_w_offset)


}

