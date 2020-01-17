# Make the regression plots
#
# This function makes the nice regression plots to show side by side presenting the results
#' @export


makeRegressionPlots <- function(modeltoplot){

    #Plot the models
    p1<-sjPlot::plot_model(modeltoplot)

    p1$data$term <- ifelse(p1$data$term == "post_length", "post length", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "useful_information_ynY", "contains useful info", as.character(p1$data$term))

    p1$data$term <- ifelse(p1$data$term == "ast_yn1", "contains asterisk", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "qm_yn1", "contains question mark", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "exc_yn1", "contains exclamation mark", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "hasht_yn1", "contains hashtag", as.character(p1$data$term))

    p1$data$term <- ifelse(p1$data$term == "sent_hopeful", "hopeful", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_hopeless", "hopeless", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_sad", "sad", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_scared", "scared", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_worried", "worried", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_neutral", "neutral", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_neg", "negative", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_wte", "willing to engage", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_concerned", "concerned", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "sent_score", "sentiment score", as.character(p1$data$term))


    p1$data$term <- ifelse(p1$data$term == "aster", "**missing**", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "appeal", "...are appealing for...", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "concern", "...are concerned for...", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "hashmiss", "#missing", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "origyn_2", "original phrasing", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "plsrt", "please RT", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "oneohone", "call 101", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "help", "can you help", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "highrisk", "high risk", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "haveuseen", "have you seen...", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "urg", "urgent appeal", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "link", "link to info", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "thx", "thanks", as.character(p1$data$term))

    p1$data$term <- ifelse(p1$data$term == "tone_codedrational", "rational tone", as.character(p1$data$term))

    p1$data$term <- ifelse(p1$data$term == "phototypeCustody photo", "custody photo", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "phototypeRegular photo", "regular photo", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "phototypeMuliple photos", "multiple photos", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "race_codedwhite", "white ethnic appearance", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "gender_codedmale", "male", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "image_quality_codedbad", "bad image quality", as.character(p1$data$term))
    p1$data$term <- ifelse(p1$data$term == "image_quality_codedgood/excellent", "good/excellent image quality", as.character(p1$data$term))

    p1$data$term <- factor(p1$data$term, levels = c("post length", "contains useful info",
                                                    "contains asterisk", "contains question mark", "contains exclamation mark", "contains hashtag",
                                                    "hopeful", "hopeless", "sad", "scared", "worried", "neutral", "negative", "willing to engage",
                                                    "concerned", "sentiment score", "original phrasing", "please RT", "**missing**",
                                                    "...are appealing for...", "#missing", "call 101", "urgent appeal", "link to info",
                                                    "thanks", "can you help", "...are concerned for...", "high risk", "have you seen...",
                                                    "rational tone", "bad image quality", "good/excellent image quality", "custody photo",
                                                    "regular photo", "multiple photos", "white ethnic appearance", "male"))


    ggplot2::ggplot(p1$data, ggplot2::aes(x = estimate, y = term)) +
      ggplot2::geom_point() +
      ggplot2::geom_segment(ggplot2::aes(x = conf.low, xend = conf.high, y = term, yend = term)) +
      ggplot2::theme_minimal() +
      ggplot2::geom_vline(aes(xintercept = 1, alpha = 0.8)) +
      ggplot2::scale_x_continuous(name = "Incidence Rate Ratios", breaks = c(0.01, 0.1, 1, 10, 100), limits = c(0,100))


    return(p1)

  }
