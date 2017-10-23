#Code for extracting data from twitter

#R has a package known as twitteR which provides access to the twitter API
install.packages("twitteR")
install.packages("bit")
require("twitteR")
library(twitteR)

install.packages("base64enc")
require("base64enc")
library(base64enc)


#Code for Authentication

#Consumer key:
api_key <- "gSV9SMy3ViwXho0Yw5igWMdcF"   

#Consumer secret:
api_secret <- "Ovxke135m6VGRFFNKsD9vGaQUs4FaJEWA5iD6BhxgD6iEerlCg"   

#Access token:
access_token <- "839584674359967744-XLW3YcG5LoLyrald51Z54gBiu8lPlk3"   

#Access token secret:
access_token_secret <- "dLwtGRmLkP69As0YDtWRXxwc48QFRGXE0B5FZFSlabiMj"  

#Sets up the OAuth credentials for a twitteR session
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Use a local file to cache OAuth access credentials. So select Yes after running the above command
#[1] "Using direct authentication"
#Use a local file to cache OAuth access credentials between R sessions?
#1: Yes
#2: No
#Selection: 1


#Code to extract Tweets
#hashtags = c('#LasVegasShootings','#lasvegasstrong', '#Trump')
hashtags = c('#LasVegas','#shootings', '#GuncontrolNow')

for (hashtag in hashtags){

    #Search of Twitter based on the search string 'hashtag' and the number of tweets to be searched  
    tweets = searchTwitter(hashtag, n=1000 )
    
    #Convert from TwiiteR lists to dataframe
    tweets = twListToDF(tweets)    
    
    #Assign tweets for cleaning
    tweets.df = tweets[,1]  
    
    tweets.df = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df);head(tweets.df) 
    
    #To remove @user
    tweets.df = gsub("@\\w+", "", tweets.df);head(tweets.df) 
    
    #Remove punctuation marks
    tweets.df = gsub("[[:punct:]]", "", tweets.df);head(tweets.df) 
    
    #Remove numbers
    tweets.df = gsub("[[:digit:]]", "", tweets.df);head(tweets.df)
    
    #Remove links
    tweets.df = gsub("http\\w+", "", tweets.df);head(tweets.df) # regex for removing links
    
    #Remove new line 
    tweets.df = gsub("\n", " ", tweets.df);head(tweets.df) 
    
    #Remove blank spaces
    tweets.df = gsub("[ \t]{2,}", " ", tweets.df);head(tweets.df) 
    
    #Keep only alpha numeric
    tweets.df =  gsub("[^[:alnum:]///' ]", " ", tweets.df)
    
    #Keep only ASCII characters
    tweets.df =  iconv(tweets.df, "latin1", "ASCII", sub="")   
    
    #Remove leading and trailing white space
    tweets.df = gsub("^\\s+|\\s+$", "", tweets.df);head(tweets.df)
    
    #Save as dataframe
    tweets[,1] = tweets.df 
    
    #View(tweets)
    dim(tweets)
    
    #Generates 3 csv files('LasVegas','shootings', 'GunControlNow')
    write.csv(tweets,paste0(gsub('#','',hashtag),'.csv'))

}
