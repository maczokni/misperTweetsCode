\name{cleanImgQual}
\title{Clean image quality tagging}
\description{
This function cleans the tagging of image quality that was done manually
}
\alias{cleanImgQual}
\usage{
cleanImgQual(x)
}
\arguments{
  \item{x}{a string representing the image quality that has been manually coded}
}

\examples{
df <- tibble::tribble(
  ~image_quality,
  "good",
   "N.P"
)
 df$image_quality_coded <- sapply(df$image_quality, cleanImgQual)
}

