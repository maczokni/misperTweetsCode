\name{cleanImgQual}
\alias{cleanImgQual}
\title{Clean image quality tagging}
\usage{
cleanMisperData(misper_tweets)
}
\description{
This function cleans the tagging of image quality that was done manually
}
\examples{
misper_tweets <- tibble::tribble(
  ~image_quality,
  "good",
   "N.P"
)
 misper_tweets$image_quality_coded <- sapply(misper_tweets$image_quality, cleanImgQual)
}

