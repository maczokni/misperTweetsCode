# make plot for figure 1

makeFig1 <- function(fig_df) {

  fig_df$phototype <- factor(fig_df$phototype, levels = c("No photo", "Custody photo", "Regular photo", "Muliple photos"))

  figure.1 <- ggplot2::ggplot(fig_df, aes(x = w_median, y = phototype)) +
    ggplot2::facet_wrap( ~ gender_coded) +
    ggplot2::geom_errorbarh(data = fig_df, aes(x = nw_median, y = phototype, xmin = nw_lqt, xmax =  nw_uqt), height = 0.1, lwd = 0.5, col = "#8A5C7B", alpha = 1, position = position_nudge(y = -0.05)) +
    ggplot2::geom_errorbarh(data = fig_df, aes(x =  w_median, y = phototype, xmin = w_lqt, xmax =  w_uqt), height = 0.1, lwd = 0.5, col = "black", alpha = 1, position = position_nudge(y = 0.05)) +
    ggplot2::geom_point(aes(fill = "White", size = w_count), pch = 21, col = "black", position = position_nudge(y = 0.05)) +
    ggplot2::geom_point(aes(x = nw_median, fill = "Ethnic minority", size = nw_count), pch = 21, col = "black", position = position_nudge(y = -0.05)) +
    ggplot2::scale_fill_manual(values = c("#8A5C7B", "white"), labels = c("Ethnic minority", "White")) +
    ggplot2::scale_alpha_continuous(guide = F) +
    ggplot2::labs(title = "",
         subtitle = "",
         caption = "") +
    ggplot2::xlab("Median retweet count") +
    ggplot2::ylab("")+
    ggplot2::theme_bw() +
    ggplot2::guides(fill = guide_legend(title = "Ethnic appearance"), size = guide_legend(title = "Number of tweets")) +
    ggplot2::theme(legend.position = c(0.87, 0.20),
          plot.title = element_text(size = 16, face = 'bold'),
          legend.background = element_rect(fill = alpha("white", 0.0)),
          strip.background = element_rect(fill="white", colour = 'white'),
          strip.text.x = element_text(size = 12,angle = 0, hjust = 0),
          strip.text.y = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          panel.grid.major = element_blank())

  return(figure.1)

}
