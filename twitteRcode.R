#LOAD IN THE NECESSARY PACAKGES
library(ggplot2)
require(plyr)  
require(stringr)
library(twitteR)
library(syuzhet)
library(quanteda)

#ACCESS TWITTERS API AND GET AUTHORIZATION

#Go to https://dev.twitter.com/ and log in with your Twitter Account.

#Now you can see your Profile picture in the upper right corner and a drop-down menu. In this menu you can find “My Applications”.

#Click on it and then on “Create new application”.

#You can name your Application whatever you want and also set Description on whatever you want. Twitter requires a valid URL for the website, you can just type in http://test.de/ ; you won´t need it anymore.

#YOU WILL NEED TO ENTER YOUR OWN NUMBERS - THESE WILL NOT WORK FOR YOU
api_key <- "OJl7tCe1OItxVUCIuuctmKhL2"
api_secret <- "Mt1GpylBSnoJwCsqxuzm32XaagGAzLKojwr0qejBjMLiOY3g1Y"
access_token <- "4129864713-6DznWrZSxVAsrYeZn96karmQ8QGz3fPHEfjScf1"
access_token_secret <- "KteMxvHAFBtPw4DKOh8bhttWHQmH0HvU0b0201M1Y3yCN"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#########SCRAPE TWEETS FROM INDIVIDUAL ACCOUNT#########
#THE MAX NUMBER OF TWEETS YOU CAN GRAB IS 3,200
TrumpTweeting <- userTimeline('realDonaldTrump', n = 3200,  includeRts = FALSE, excludeReplies = TRUE)

#CREATE A DATAFRAME
df <- do.call(rbind, lapply(TrumpTweeting, as.data.frame))
#ADJUST THE TIME VARIABLE SO WE CAN PLOT TRENDS OVER TIME
df$created <- strftime(df$created, '%Y-%m-%d')
df$created2 <- as.Date(df$created, '%Y-%m-%d')

#PLOT THE NUMBER OF RETWEETS OF TRUMP'S LAST 3200 TWEETS
ggplot(data=df, aes(x=created2, y=retweetCount)) + geom_point() + geom_smooth(aes(group=1), se = FALSE) + scale_x_date()


#CREATE A SENTIMENT SCORE FOR EACH TWEET USING THE "szyuhet" PACKAGE
df$text <- str_replace_all(df$text,"[^[:graph:]]", " ")
df$sentiment <- get_sentiment(df$text, method="afinn")

#PLOT THE SENTIMENT OF TRUMP'S LAST 3200 TWEETS
ggplot(data=df, aes(x=created, y=sentiment)) + geom_point() + geom_smooth(aes(group=1), se = FALSE) 

#PLOT THE DISTRIBUTION OF THE SENTIMENT VARIABLE
ggplot(df, aes(x=sentiment)) + geom_histogram(binwidth=1, fill="#56B4E9", colour="black") + theme_classic() +  theme(panel.background = element_rect(colour = 'black', size = 1, linetype='solid'))  +  labs(x="Sentiment Score of Tweets",y="Frequency") 

#########SCRAPE TWEETS FROM INDIVIDUAL ACCOUNT#########
#THE MAX NUMBER OF TWEETS YOU CAN GRAB IS 3,200
CZTweeting <- userTimeline('christinezhang', n = 3200,  includeRts = FALSE, excludeReplies = TRUE)

#CREATE A DATAFRAME
df <- do.call(rbind, lapply(CZTweeting, as.data.frame))
#ADJUST THE TIME VARIABLE SO WE CAN PLOT TRENDS OVER TIME
df$created <- strftime(df$created, '%Y-%m-%d')
df$created2 <- as.Date(df$created, '%Y-%m-%d')

#PLOT THE NUMBER OF RETWEETS OF TRUMP'S LAST 3200 TWEETS
ggplot(data=df, aes(x=created2, y=retweetCount)) + geom_line() + geom_smooth(aes(group=1), se = FALSE) + scale_x_date()


#CREATE A SENTIMENT SCORE FOR EACH TWEET USING THE "szyuhet" PACKAGE
df$text <- str_replace_all(df$text,"[^[:graph:]]", " ")
df$sentiment <- get_sentiment(df$text, method="afinn")

#PLOT THE SENTIMENT OF TWEETS 
ggplot(data=df, aes(x=created2, y=sentiment)) + geom_point() + geom_smooth(aes(group=1), se = FALSE) + scale_x_date() + ggtitle("The Sentiment of Christine Zhang's Tweets") +  xlab("Date")
 

#########SEARCH FOR TWEETS CONTAINING WORDS###########
Trump_tweets <-searchTwitter('Trump', n=500, lang="en")

df <- do.call(rbind, lapply(Trump_tweets, as.data.frame))
write.csv(df, file = "TrumpTweets.csv")

#Load csv file with 3000 tweets that contain the word "Trump." 
#the individual tweets are in a column called "text"
TrumpTweets <- textfile("TrumpTweets.csv", textField = "text")

#Create a corpus of the tweets
twitterCorpus <- corpus(TrumpTweets) 
#Summarize the corpus
summary(twitterCorpus, n=5)

#Clean up the tweets for processing  
review_text <- str_replace_all(twitterCorpus$documents$texts,"[^[:graph:]]", " ")
#Create a new document-level variable for sentiment score
docvars(twitterCorpus, "Sentiment") <- get_sentiment(review_text, method="afinn")

#Create a data frame called "tokenTweets"
tokenTweets <- summary(twitterCorpus, n=3000)

#Plot the distribution of the "tokenTweets" varaible 
ggplot(tokenTweets, aes(x=Sentiment)) + geom_histogram(binwidth=1, fill="#56B4E9", colour="black") + theme_classic() +  theme(panel.background = element_rect(colour = 'black', size = 1, linetype='solid'))  +  labs(x="Sentiment Score of Tweets",y="Frequency") 

