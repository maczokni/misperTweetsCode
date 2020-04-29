# make hashtag type dataframe

makeHashDf <- function(misper_tweets) {

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


  #make df
  hash_list <- c("h_location", "h_missing", "h_pol_num", "h_other")
  datalist = list()
  i <- 1
  for (hash in hash_list) {
    datalist[[i]] <- data.frame(hash = gsub("h_", "", hash),
                                mean_rt = mean(misper_tweets %>% filter(eval(as.symbol(hash)) == 1) %>% pull(retweet_count), na.rm = TRUE),
                                sd_rt = sd(misper_tweets %>% filter(eval(as.symbol(hash)) == 1) %>% pull(retweet_count), na.rm = TRUE),
                                median_rt = median(misper_tweets %>% filter(eval(as.symbol(hash)) == 1) %>% pull(retweet_count), na.rm = TRUE),
                                iqr = paste0(quantile(misper_tweets %>% filter(eval(as.symbol(hash)) == 1) %>% pull(retweet_count), na.rm = TRUE)[2], "-",quantile(misper_tweets %>% filter(eval(as.symbol(hash)) == 1) %>% pull(retweet_count), na.rm = TRUE)[4]),
                                num_tweets_with = nrow(misper_tweets %>% filter(eval(as.symbol(hash)) == 1)),
                                median_rt_not = median(misper_tweets %>% filter(eval(as.symbol(hash)) == 0) %>% pull(retweet_count), na.rm = TRUE),
                                low_qt = quantile(misper_tweets %>% filter(eval(as.symbol(hash)) == 1) %>% pull(retweet_count), na.rm = TRUE)[2],
                                up_qt = quantile(misper_tweets %>% filter(eval(as.symbol(hash)) == 1) %>% pull(retweet_count), na.rm = TRUE)[4],
                                low_qt_not = quantile(misper_tweets %>% filter(eval(as.symbol(hash)) == 0) %>% pull(retweet_count), na.rm = TRUE)[2],
                                up_qt_not = quantile(misper_tweets %>% filter(eval(as.symbol(hash)) == 0) %>% pull(retweet_count), na.rm = TRUE)[4],
                                num_tweets_not = nrow(misper_tweets %>% filter(eval(as.symbol(hash)) == 0))


    )
    i <- i + 1


  }

  hash_df <- bind_rows(datalist)

  hash_df$hash <- ifelse(hash_df$hash == "missing", "#missing", hash_df$hash)
  hash_df$hash <- ifelse(hash_df$hash == "pol_num", "police force or number", hash_df$hash)


  return(hash_df)

}
