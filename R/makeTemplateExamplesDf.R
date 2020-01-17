# make template examples dataframe for table 7

makeTemplateExamplesDf <- function(misper_tweets) {

  template_list <- c("oneohone", "help", "concern", "plsrt", "hashmiss", "aster", "appeal", "highrisk", "urg", "link", "origyn_2", "haveuseen", "thx")

  datalist = list()
  i <- 1
  for (temp in template_list) {
    datalist[[i]] <- data.frame(template = temp,
                                num_tweets = nrow(misper_tweets %>% dplyr::filter(eval(as.symbol(temp)) == 1)),
                                example = misper_tweets %>% dplyr::filter(eval(as.symbol(temp)) == 1) %>% dplyr::sample_n(size=1) %>% dplyr::pull(text))
    i <- i + 1


  }

  examples <- dplyr::bind_rows(datalist)

  examples$template <- ifelse(examples$template == "urg", "urgent appeal", examples$template)
  examples$template <- ifelse(examples$template == "plsrt", "please RT", examples$template)
  examples$template <- ifelse(examples$template == "origyn_2", "original phrasing", examples$template)
  examples$template <- ifelse(examples$template == "link", "link to info", examples$template)
  examples$template <- ifelse(examples$template == "oneohone", "call 101", examples$template)
  examples$template <- ifelse(examples$template == "highrisk", "high risk", examples$template)
  examples$template <- ifelse(examples$template == "help", "can you help", examples$template)
  examples$template <- ifelse(examples$template == "hashmiss", "#missing", examples$template)
  examples$template <- ifelse(examples$template == "concern", "... are concerned for..", examples$template)
  examples$template <- ifelse(examples$template == "aster", "\\*\\*missing\\*\\*", examples$template)
  examples$template <- ifelse(examples$template == "appeal", "... are appealing for..", examples$template)
  examples$template <- ifelse(examples$template == "haveuseen", "have you seen..", examples$template)
  examples$template <- ifelse(examples$template == "thx", "thanks", examples$template)

  examples$example <- gsub("\t", " ", examples$example)
  examples$example <- gsub("\n", " ", examples$example)
  examples$example <- gsub("*", "\\*", examples$example, fixed = TRUE)

  return(examples)

}
