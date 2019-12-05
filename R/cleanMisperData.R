# Clean missing persons data
#
# This function cleans the data by removing duplicates, and by cleaning up any errors in manual tagging
# It also codes the gender and ethnic appearance based on photo or text where photo is not available
# and creates any new variables required for the analysis such as post length, etc
#' @export

cleanMisperData <- function(misper_tweets){
  misper_tweets <- as.data.frame( misper_tweets)
  #remove duplicates
  misper_tweets <- misper_tweets[order(misper_tweets[,'text'],-misper_tweets[,'retweet_count']),]
  misper_tweets <- misper_tweets[!duplicated(misper_tweets$text),]

  #DATE AND HOUR
  #format date as date
  misper_tweets$date <- lubridate::dmy_hm(misper_tweets$created_at)
  #get hour
  misper_tweets$tw_hour <- as.factor(lubridate::hour(misper_tweets$date))

  #AGE OF TWEET
  misper_tweets$collection_date <- max(misper_tweets$date)
  misper_tweets$diffdate <- round(difftime(misper_tweets$collection_date, misper_tweets$date, units = "days"),0)
  misper_tweets$diffdate <- as.numeric(as.character(misper_tweets$diffdate))


  #CREATE VARIABLES FOR RACE/ETHNIC APPEARANCE
  #if there is a photo we code from that
  #if no photo, we take it from text
  #if no photo and not in text, we code as NA
  #if tweet is about more than one person and they are different ethnicity, we code as NA (exclude)
  misper_tweets$race_coded <- "non-white"
  misper_tweets$race_coded <- ifelse(grepl("whi", misper_tweets$race_picture), "white", misper_tweets$race_coded)
  misper_tweets$race_coded <- ifelse(grepl(",", misper_tweets$race_picture) |
                                       misper_tweets$race_picture == "" |
                                       misper_tweets$race_picture == "N.P", NA, misper_tweets$race_coded)

  misper_tweets$race_coded <- ifelse(is.na(misper_tweets$race_coded) & grepl("whi", misper_tweets$race_text), "white", misper_tweets$race_coded)
  l  <- c("African",    "asian",      "black",      "greek",      "indian",     "mixed",      "mixed race", "mixed-race", "polish", "vietnamese")
  misper_tweets$race_coded <- ifelse(is.na(misper_tweets$race_coded) &
                                       misper_tweets$race_text %in% l, "non-white", misper_tweets$race_coded)



  #CREATE VARIABLES FOR GENDER
  #code gender from photo or text where no photo was available
  misper_tweets$gender_coded <- NA
  misper_tweets$gender_coded <- ifelse(grepl("F", misper_tweets$gender_picture), "Female", misper_tweets$gender_coded)
  misper_tweets$gender_coded <- ifelse(grepl("M", misper_tweets$gender_picture) , "Male", misper_tweets$gender_coded)
  misper_tweets$gender_coded <- ifelse(grepl(",", misper_tweets$gender_picture)  , "multippl", misper_tweets$gender_coded)
  misper_tweets$gender_coded <- ifelse(grepl("N.P", misper_tweets$gender_picture) |
                                         #   grepl(",", misper_tweets$gender_picture) |
                                         misper_tweets$gender_picture == "" |
                                         misper_tweets$gender_picture == "N.P", NA, misper_tweets$gender_coded)

  misper_tweets$gender_coded <- ifelse(is.na(misper_tweets$gender_coded) & grepl("F", misper_tweets$gender_text), "Female", misper_tweets$gender_coded)
  misper_tweets$gender_coded <- ifelse(is.na(misper_tweets$gender_coded) & grepl("M", misper_tweets$gender_text), "Male", misper_tweets$gender_coded)

  #create variable for photo type
  #code photo as either no photo, custody photo, normal photo, multiple photos
  misper_tweets$phototype <- "Regular photo"
  misper_tweets$phototype <- ifelse(grepl('N.P', misper_tweets$image_type), "No photo", misper_tweets$phototype)
  misper_tweets$phototype <- ifelse(grepl(paste0('^([0-9])', ' photos'), misper_tweets$image_type), "Muliple photos", misper_tweets$phototype)
  misper_tweets$phototype <- ifelse(grepl('custody', misper_tweets$image_type), "Custody photo", misper_tweets$phototype)
  table(misper_tweets$phototype)
  misper_tweets$phototype <- factor(misper_tweets$phototype, levels = c("No photo", "Custody photo", "Regular photo", "Muliple photos"))

  #create variable for photo quality
  misper_tweets$image_quality_coded <- sapply(misper_tweets$image_quality, misperTweetsCode:::cleanImgQual)
  misper_tweets$image_quality_coded <- ifelse(misper_tweets$image_quality_coded == "idk", NA, misper_tweets$image_quality_coded)

  #POST LENGTH
  misper_tweets$post_length <- nchar(as.character(misper_tweets$text))

  #PUNCT
  misper_tweets$numexc <- misperTweetsCode:::countCharOccurrences("\\!", as.character(misper_tweets$text))
  misper_tweets$numqm <- misperTweetsCode:::countCharOccurrences("\\?", as.character(misper_tweets$text))
  misper_tweets$numast <- misperTweetsCode:::countCharOccurrences("\\*", as.character(misper_tweets$text))
  misper_tweets$numhash <- misperTweetsCode:::countCharOccurrences("\\#", as.character(misper_tweets$text))
  misper_tweets$ast_yn <- as.factor(ifelse(misper_tweets$numast >0, 1, 0))
  misper_tweets$qm_yn <- as.factor(ifelse(misper_tweets$numqm >0, 1, 0))
  misper_tweets$exc_yn <- as.factor(ifelse(misper_tweets$numexc >0, 1, 0))
  misper_tweets$hasht_yn <- as.factor(ifelse(misper_tweets$numhash >0, 1, 0))

  #HASHTAG
  misper_tweets$coded_hash_type <- misper_tweets$hashtag_type
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "N.P",NA, misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place","location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "missing","missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "missing, place","location,missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "?","other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place, missing","location,missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "missing ","missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place ","location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "appreciation","appreciation", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "event ","location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "missing, location, place","missing,location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "number","number", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "number ","number", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place, action","location,action", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place, missing, missing person, action, misper, place where misper is missing from","location,action,missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place, missing, place where the misper is missing from","location,missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "police","police", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "policestation, place","police,location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "search","search", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "appeal, name of missing person","appeal,name", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "charity","other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "information","other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "missing person","missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "missing, location where they are missing from","missing,location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "missing, missing person","missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "name","other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "name of concert","event", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "name of missing person, request for help finding misper","other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "news","other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place where the misper is missing from","location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place, find+name of missing person","location,other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place, location, missing","missing,location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place, missing ","missing,location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "place, missing, missing person, place where the misper is missing from","missing,location", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "police department","police", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "police, policestation, place, missing","police,missing", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "policestation","police", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "policestation ","police", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "request, ?","other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "risk, capital letters","other", misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(misper_tweets$hashtag_type == "search", "other",misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(grepl("Ramsbottom", misper_tweets$hashtags) |
                                            grepl("Salford", misper_tweets$hashtags) |
                                            grepl("Greece", misper_tweets$hashtags) |
                                            grepl("Greece", misper_tweets$hashtags) |
                                            grepl("Eccles", misper_tweets$hashtags), "location" , misper_tweets$coded_hash_type)

  misper_tweets$coded_hash_type <- ifelse(grepl("MissingPersonAppeal", misper_tweets$hashtags), "missing",  misper_tweets$coded_hash_type)
  misper_tweets$coded_hash_type <- ifelse(grepl("Crimestoppers", misper_tweets$hashtags), "police",  misper_tweets$coded_hash_type)

  misper_tweets$h_location <- ifelse(grepl("location", tolower(misper_tweets$coded_hash_type))|
                                       grepl("event", tolower(misper_tweets$coded_hash_type)), 1, 0)
  misper_tweets$h_missing <- ifelse(grepl("missing", tolower(misper_tweets$coded_hash_type)), 1, 0)
  misper_tweets$h_pol_num <- ifelse(grepl("police", tolower(misper_tweets$coded_hash_type))|
                                      grepl("number", tolower(misper_tweets$coded_hash_type)), 1, 0)
  misper_tweets$h_other <- ifelse(grepl("other", tolower(misper_tweets$coded_hash_type))|
                                    grepl("appeal", tolower(misper_tweets$coded_hash_type))|
                                    grepl("name", tolower(misper_tweets$coded_hash_type)) |
                                    grepl("appreciation", tolower(misper_tweets$coded_hash_type))|
                                    grepl("action", tolower(misper_tweets$coded_hash_type))
                                  , 1, 0)


  #QUANT SENT
  sent <- misper_tweets %>% dplyr::select(text)
  sent$text <- as.character(sent$text)

  sent <- sent %>%
    tidytext::unnest_tokens(word, text, drop = FALSE) %>%
    dplyr::inner_join(tidytext::get_sentiments("afinn")) %>%
    dplyr::group_by(text) %>%
    dplyr::summarise(sent_score = sum(value, na.rm = T))

  misper_tweets <- dplyr::left_join(misper_tweets, sent, by = c("text" = "text"))

  #QUAL SENT
  misper_tweets$clean_sent <- as.character(misper_tweets$sentiment)
  misper_tweets$clean_sent <- gsub("neutral","neutral", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("worried","worried", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sadness","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("scared","scared", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerned","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("urgency","urgency", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("very worried","worried", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingnesss to engage","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("negativity","negative", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopeful","hopeful", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("strong sadness","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("strongly worried","worried", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingnes to engage","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("person? Curious","curious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingness to engage","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopefull","hopeful", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hf","hf", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willing to engage","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopeless","hopeless", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("confused","confused", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?","missing?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("curious","curious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concern","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("","", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("inquisitive","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("funny","funny", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("wmi","wmi", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("uncertainty","uncertain", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerend","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("person?","person?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerend. Confused","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopelss","hopeless", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("safe","safe", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingness o engage","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("inquisitie","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("inqisitive","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("suspicious","suspicious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerened","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("fh","fh", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerned scared","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?","found?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("ngativity","negative", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sandess","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("nagativity","negative", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sdness","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("worrid","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("inqusitive","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("anxious","anxious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("Inquisitive","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingness to enageg","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("conerened","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("negatovoty","negative", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found? Hf","hf", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found? Safe","safe", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingness to enagage","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("conccerned","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("safe hopeful","safe", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hoplesness","hopeless", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("angry","angry", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingness to help","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("will","will", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopful","hopeful", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("strong sadnesss","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("do","do", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("impressed","impressed", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("inquisitve","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("where","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("try","try", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerne","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernes","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("nagtivity","negative", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sadnss","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopeful concerned","hopeful", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("when","when", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing? Hf","hf", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("mising?","missing?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("positive","positive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("Wmi","wmi", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("where?","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("negtivity","negative", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerrned","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("will. Inquisitive","will", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sfe","sfe", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("were","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing? Concerned","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("foun?","found?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing? Inquisitive","inquisitive", misper_tweets$clean_sent)

  misper_tweets$clean_sent <- gsub("angry","angry", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("anxious","anxious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerned","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernedd","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernedd. Confused","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernedde","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernedded","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernedded scared","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerneddes","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("confused","confused", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("curious","curious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("do","do", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("fh","fh", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?d??","found?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?d?? Hf","found?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?d?? Safe","found?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("funny","funny", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hf","hf", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopeful","hopeful", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopefulded","hopeful", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopeless","hopeless", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("impressed","impressed", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("inquisitive","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing??","missing?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?? Concerned","missing?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?? Hf","missing?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?? inquisitive","missing?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("negative","negative", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("neutral","neutral", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("person??","person?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("person?? Curious","person?", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("positive","positive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sad","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("safe","safe", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("scared","scared", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sfe","sfe", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("strong sad","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("strong sads","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("suspicious","suspicious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("try","try", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("uncertain","uncertain", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("urgency","urgency", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("when","when", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("where","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("where?","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("will","will", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("will. inquisitive","will", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingnesss to engage","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("wmi","wmi", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("worried","worried", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("anxious","anxious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?d?d?? Hf","found", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?d?d?? Safe","found", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("angry","angry", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("will","will", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sads","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("do","do", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("impressed","impressed", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("where","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("try","try", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernede","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernedes","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("when","when", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?g?? Hf","missing person", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("positive","positive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("where?","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sfe","safe", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?g?? Concerned","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?g?? inquisitive","inquisitive", misper_tweets$clean_sent)
  #misper_tweets$clean_sent <- ifelse(grepl("?", misper_tweets$clean_sent), NA, misper_tweets$clean_sent)

  misper_tweets$clean_sent <- gsub("neutral","neutral", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("worried","worried", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sad","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("scared","scared", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerneded","concerneded", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("urgency","urgency", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("willingnesss to engage","willingnesss to engage", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("negative","negative", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopeful","hopeful", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("person?n?? Curious","curious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hf","hf", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("hopeless","hopeless", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("confused","confused", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?g??","missing person", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("curious","curious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerned","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("","", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("inquisitive","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("funny","funny", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("wmi","wmi", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("uncertain","uncertain", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("person?n??","missing person", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerned. Confused","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("safe","", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("suspicious","suspicious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("fh","fh", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concerneded scared","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?d?d??","found", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("anxious","anxious", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?d?d?? Hf","found", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("found?d?d?? Safe","found", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("angry","angry", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("will","will", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sads","sad", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("do","do", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("impressed","impressed", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("where","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("try","try", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernede","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernedes","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("when","when", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?g?? Hf","missing person", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("positive","positive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("where?","where", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("sfe","safe", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?g?? Concerned","concerned", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("missing?g?? inquisitive","inquisitive", misper_tweets$clean_sent)
  misper_tweets$clean_sent <- gsub("concernedd","concerned", misper_tweets$clean_sent)

  #group into the main sentiments
  misper_tweets$sent_neutral <- ifelse(grepl("neutral", misper_tweets$clean_sent), 1, 0)
  misper_tweets$sent_worried <- ifelse(grepl("worr", misper_tweets$clean_sent), 1, 0)
  misper_tweets$sent_sad <- ifelse(grepl("sad", misper_tweets$clean_sent), 1, 0)
  misper_tweets$sent_scared <- ifelse(grepl("scare", misper_tweets$clean_sent), 1, 0)
  misper_tweets$sent_concerned <- ifelse(grepl("conc", misper_tweets$clean_sent), 1, 0)
  misper_tweets$sent_wte <- ifelse(grepl("will", misper_tweets$clean_sent), 1, 0)
  misper_tweets$sent_neg <- ifelse(grepl("neg", misper_tweets$clean_sent), 1, 0)
  misper_tweets$sent_hopeful <- ifelse(grepl("hopef", misper_tweets$clean_sent), 1, 0)
  misper_tweets$sent_hopeless <- ifelse(grepl("hopel", misper_tweets$clean_sent), 1, 0)

  #USEFUL INFO
  misper_tweets$useful_information_yn <- ifelse(misper_tweets$useful_information == "N", "N", "Y")

  #TONE
  misper_tweets$tone_emot <- ifelse(grepl("emo", misper_tweets$tone_type_of_appeal), 1, 0)
  misper_tweets$tone_coded <- ifelse(misper_tweets$tone_emot == 1, "emotional", "rational")

  #TEMPLATE
  misper_tweets$oneohone <- ifelse(grepl("101", tolower(misper_tweets$text)) |
                                     grepl("call", tolower(misper_tweets$text)) |
                                     grepl("contact", tolower(misper_tweets$text)), 1, 0)
  misper_tweets$help <- ifelse(grepl("can you help", tolower(misper_tweets$text)), 1, 0)
  misper_tweets$concern <- ifelse(grepl("concern", tolower(misper_tweets$text)), 1, 0)
  misper_tweets$plsrt <- ifelse(grepl("please rt", tolower(misper_tweets$text)) |
                                  grepl("please share", tolower(misper_tweets$text)) |
                                  grepl("please can you rt", tolower(misper_tweets$text)) |
                                  grepl("pls rt", tolower(misper_tweets$text)) |
                                  grepl("rt please", tolower(misper_tweets$text)), 1, 0)
  misper_tweets$hashmiss <- ifelse(grepl("#missing", tolower(misper_tweets$text)) , 1, 0)
  misper_tweets$aster <- ifelse(grepl("\\*m", tolower(misper_tweets$text)) , 1, 0)
  misper_tweets$appeal <- ifelse(grepl("are appealing for", tolower(misper_tweets$text)), 1, 0)
  misper_tweets$haveuseen <- ifelse(grepl("have you seen", tolower(misper_tweets$text)) , 1, 0)
  misper_tweets$highrisk <- ifelse(grepl("high risk", tolower(misper_tweets$text)) , 1, 0)
  misper_tweets$thx <- ifelse(grepl("thank", tolower(misper_tweets$text)) , 1, 0)
  misper_tweets$urg <- ifelse(grepl("urgent", tolower(misper_tweets$text)) , 1, 0)
  misper_tweets$link <- ifelse(grepl("attach", tolower(misper_tweets$text)) |
                                 grepl("link", tolower(misper_tweets$text)), 1, 0)
  misper_tweets$origyn_2 <- ifelse(misper_tweets$oneohone +
                                     misper_tweets$help +
                                     misper_tweets$concern +
                                     misper_tweets$plsrt +
                                     misper_tweets$hashmiss +
                                     misper_tweets$aster +
                                     misper_tweets$appeal +
                                     misper_tweets$highrisk +
                                     misper_tweets$urg +
                                     misper_tweets$link +
                                     misper_tweets$haveuseen > 0, 0, 1)

  return(misper_tweets)

}
