# Make the df needed to build figure 1
#
# This function makes the dataframe needes form the misper tweets for us to be able to build the visualisation that compares
# photo type and retweeting broken down by gender and ethnic appearance.
#' @export


makeDemophotoplotDf <- function(misper_tweets) {
  fig_df_1 <- misper_tweets %>%
    dplyr::group_by(phototype, gender_coded, race_coded) %>%
    dplyr::summarise(mean_rt = mean(retweet_count, na.rm = TRUE)) %>%
    tidyr::spread(race_coded, mean_rt) %>%
    dplyr::select(-`<NA>`)  %>%
    dplyr::rename(nw_mean = `non-white`,
           w_mean = white)

  fig_df_2 <- misper_tweets %>%
    dplyr::group_by(phototype, gender_coded, race_coded) %>%
    dplyr::summarise(median_rt = median(retweet_count, na.rm = TRUE)) %>%
    tidyr::spread(race_coded, median_rt) %>%
    dplyr::select(-`<NA>`) %>%
    dplyr::rename(nw_median = `non-white`,
           w_median = white)

  fig_df_3 <- misper_tweets %>%
    dplyr::group_by(phototype, gender_coded, race_coded) %>%
    dplyr::summarise(num_tweets = dplyr::n()) %>%
    tidyr::spread(race_coded, num_tweets) %>%
    dplyr::select(-`<NA>`) %>%
    dplyr::rename(nw_count = `non-white`,
           w_count = white)

  fig_df_4 <- misper_tweets %>%
    dplyr::group_by(phototype, gender_coded, race_coded) %>%
    dplyr::summarise(low_qt = quantile(retweet_count, na.rm = T)[2]) %>%
    tidyr::spread(race_coded, low_qt) %>%
    dplyr::select(-`<NA>`) %>%
    dplyr::rename(nw_lqt = `non-white`,
           w_lqt = white)

  fig_df_5 <- misper_tweets %>%
    dplyr::group_by(phototype, gender_coded, race_coded) %>%
    dplyr::summarise(up_qt = quantile(retweet_count, na.rm = T)[4]) %>%
    tidyr::spread(race_coded, up_qt) %>%
    dplyr::select(-`<NA>`) %>%
    dplyr::rename(nw_uqt = `non-white`,
           w_uqt = white)

  fig_df <- dplyr::left_join(fig_df_1, fig_df_2)
  fig_df <- dplyr::left_join(fig_df, fig_df_3)
  fig_df <- dplyr::left_join(fig_df, fig_df_4)
  fig_df <- dplyr::left_join(fig_df, fig_df_5)

  #remove tweets of more than 1 person, and with no known gender
  fig_df <- fig_df %>% dplyr::filter(!is.na(gender_coded) & gender_coded != "multippl")

  return(fig_df)

}
