\name{makeTemplateExamplesDf}
\title{Make the df needed for table 7 with template examples}
\alias{makeTemplateExamplesDf}
\usage{
makeTemplateExamplesDf(misper_tweets)
}
\arguments{
  \item{misper_tweets}{cleaned data set of missing persons tweets with all variables coded manually}
}
\description{
This function creates the dataframe needed to be able to show the different wording templates and pull random examples for each as well as count how many there are.
}

\examples{
misper_tweets <- structure(list(retweet_count = c(41L, 1L, 2L, 85L, 3L, 0L, 0L,
2L, 3L, 12L), oneohone = c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0),
help = c(0,  0, 1, 0, 0, 0, 0, 0, 0, 0),
concern = c(0, 0, 0, 0, 1, 0, 0,  1, 1, 0),
plsrt = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
hashmiss = c(1,  0, 0, 0, 0, 0, 0, 0, 0, 0),
aster = c(0, 0, 1, 0, 0, 0, 0, 0,  0, 0),
appeal = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
highrisk = c(0,  0, 0, 1, 0, 0, 0, 0, 0, 0),
urg = c(0, 0, 0, 0, 0, 0, 1, 0, 0,  0),
link = c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0),
origyn_2 = c(0, 1,  0, 1, 0, 1, 1, 0, 0, 1),
haveuseen = c(0, 1, 1, 0, 0, 0, 0, 0,  0, 0),
thx = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0), text = c("... grey cardigan, peach top, blue jeans peach pumps.  Any sightings pls call GMP on 101... pls RT\n#missing",
"..missing from Bowdon. Blonde hair, white berry style hat, dark blue coat with white stripes...",
"'Deschanelle Walters, 17, was last seen 27/02/16 @ 10pm.  If seen call 101. See next tweet for her description &gt;&gt;&gt; https://t.co/CXxL760V4l",
"'Family and friends appeal for help to find Caspar Blackburn, 22, missing after a night out in Sale' - MEN report: https://t.co/dDd3T2jbkk",
"\"@gmpolice: Police are growing increasingly concerned of the whereabouts of a missing teenager from Bury. http://t.co/VBa6RWhFyM\"",
"“@ChInsDonnaAllen: Police Plea in hunt for missing Bolton Gtr Manchester man http://t.co/fBgHdT4a7P”",
"“@GMPBlackley: Appeal to trace missing mother of six, Kimberley Brown, last seen in July 2012 http://t.co/3qtZzAyeTY”",
"“@GMPBlackley: GM Police are growing increasingly concerned for the welfare of a mother-of-three reported missing\n http://t.co/F4pr4Akm15”",
"“@gmpolice: Police are growing increasingly concerned for missing David Roche from Wardley http://t.co/RZRnpOQcl0”",
"“Appeal to Ramsbottom Music Festival-goers after 18-year-old girl Kayleigh Hadfield goes missing http://t.co/WTomN25of8” @AutomaticCafe RT?"
)), row.names = c(NA, 10L), class = "data.frame")

example <- makeTemplateExamplesDf(misper_tweets)
}

