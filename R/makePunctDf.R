# make punctuation dataframe

makePunctDf <- function(misper_tweets) {

  punct_list <- c("ast_yn", "qm_yn", "exc_yn", "hasht_yn")
  datalist = list()
  i <- 1
  for (punct in punct_list) {
    datalist[[i]] <- data.frame(punct = gsub("_yn", "", punct),
                                mean_rt_with = mean(misper_tweets %>% filter(eval(as.symbol(punct)) == 1) %>% pull(retweet_count), na.rm = TRUE),
                                median_rt_with = median(misper_tweets %>% filter(eval(as.symbol(punct)) == 1) %>% pull(retweet_count), na.rm = TRUE),
                                sd_rt_with = sd(misper_tweets %>% filter(eval(as.symbol(punct)) == 1) %>% pull(retweet_count), na.rm = TRUE),
                                num_tweets_with = nrow(misper_tweets %>% filter(eval(as.symbol(punct)) == 1)),
                                mean_rt_not = mean(misper_tweets %>% filter(eval(as.symbol(punct)) == 0) %>% pull(retweet_count), na.rm = TRUE),
                                median_rt_not = median(misper_tweets %>% filter(eval(as.symbol(punct)) == 0) %>% pull(retweet_count), na.rm = TRUE),
                                sd_rt_not = sd(misper_tweets %>% filter(eval(as.symbol(punct)) == 0) %>% pull(retweet_count), na.rm = TRUE),
                                low_qt_with = quantile(misper_tweets %>% filter(eval(as.symbol(punct)) == 1) %>% pull(retweet_count), na.rm = TRUE)[2],
                                up_qt_with = quantile(misper_tweets %>% filter(eval(as.symbol(punct)) == 1) %>% pull(retweet_count), na.rm = TRUE)[4],
                                low_qt_not = quantile(misper_tweets %>% filter(eval(as.symbol(punct)) == 0) %>% pull(retweet_count), na.rm = TRUE)[2],
                                up_qt_not = quantile(misper_tweets %>% filter(eval(as.symbol(punct)) == 0) %>% pull(retweet_count), na.rm = TRUE)[4],
                                num_tweets_not = nrow(misper_tweets %>% filter(eval(as.symbol(punct)) == 0))

    )
    i <- i + 1


  }

  punct_df <- bind_rows(datalist)

  punct_df$punct <- ifelse(punct_df$punct == "ast", "asterisk (*)", punct_df$punct)
  punct_df$punct <- ifelse(punct_df$punct == "qm", "question mark (?)", punct_df$punct)
  punct_df$punct <- ifelse(punct_df$punct == "exc", "exclamation mark (!)", punct_df$punct)
  punct_df$punct <- ifelse(punct_df$punct == "hasht", "hashtag (#)", punct_df$punct)

  return(punct_df)

}
