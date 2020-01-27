# misperTweetsCode

This is the code to accompany the paper 'Exploring public engagement with missing person appeals on Twitter.' The pre-print of the paper can be found on the Open Science Foundation page for this project here:  [https://osf.io/4w5eg/](https://osf.io/4w5eg/). This project aimed to explore the way in which appeals for missing persons are constructed on Twitter by Greater Manchester Police, and how the public engage with these Tweets.


We identified features associated with public engagement with Tweets in a literature review, and then used 1,008 Tweets made by Greater Manchester Police to analyse the presence of these features, as well as the number of retweets of the Tweet messages.


To access the code you can download this package into R directly with `devtools::install_github("maczokni/misperTweetsCode")`.


The functions in this package are all called within the RMarkdown document which builds the paper. This is found in this repo under final_paper.Rmd. In order for this to knit, you will have to download the anonymised data from the OSF page. The data to run this is now available on the OSF page for this project here: [https://osf.io/4w5eg/](https://osf.io/4w5eg/). 


The data was collected using the Twitter API to get tweets from Greater Manchester Police twitter accounts, and was anonymised using  NETANOS [(Kleinberg & Mozes, 2017, Web-based text anonymization with Node.js: Introducing NETANOS (Named entity-based Text Anonymization for Open Science), Journal of Open Source Software, 2(14), 293, doi:10.21105/joss.00293)](https://joss.theoj.org/papers/10.21105/joss.00293).


This project was funded This project was funded by the [Manchester Statistical Society Campion Grant](https://manstatsoc.org/campion-grants/). 


Any questions or comments please contact Reka Solymosi on Twitter (@r_solymosi) or by email on reka.solymosi@manchester.ac.uk.
