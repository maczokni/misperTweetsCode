\name{cleanMisperData}
\alias{cleanMisperData}
\title{Clean missing persons data}
\usage{
cleanMisperData(misper_tweets)
}
\description{
This function cleans the data by removing duplicates, and by cleaning up any errors in manual tagging
 It also codes the gender and ethnic appearance based on photo or text where photo is not available
 and creates any new variables required for the analysis such as post length, etc
}
\examples{
misper_tweets <- tibble::tribble(
  ~user_id, ~created_at, ~screen_name, ~text, ~favorite_count, ~retweet_count, ~hashtags, ~followers_count, ~account_created_at, ~sentiment, ~hashtag_type, ~image_type, ~image_quality, ~tone_type_of_appeal, ~originiality, ~useful_information, ~gender_text, ~gender_picture, ~race_text, ~race_picture,
  NA, "09/01/2019 18:21", "gmpolice", "**Have you seen Michael?**We're still searching for 38-year-old Michael Scully - who was reported missing from Hartlepool a week ago.Michael is a street performer, you may have seen him juggling", 0L, 116L,          "", 576560L, "25/02/2009 11:43", "neutral",    "N.P", "colour, portrait", "good",                    "rational", "T1", "Y ( missing from Hartlepool, Michael is a street performer, you may have seen him juggling or flame-throwing and he busked)", "M", "", "N.P", "white ",
  NA, "09/01/2019 09:49", "gmpolice",                     "We are appealing for help in finding missing 30 y/o Carl Francis from #Southport, last seen on Wed, 2 Jan. It is believed he may be in the #Wigan or #WestYorkshire areas. Please contact @MerPolCC, 101 or @missingpeople with info. Full details: https://t.co/dFGRyW8p6X https://t.co/poM53vFlm6", 0L, 154L, "Southport", 576560L, "25/02/2009 11:43", "worried", "place ",              "N.P",  "N.P", "emotional & call for action",  "T",                                                                                                                           "N", "M", "", "N.P",    "N.P"
)
misper_tweets <- cleanMisperData(misper_tweets)
}

