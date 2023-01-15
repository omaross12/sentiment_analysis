library(ROAuth)
library(RCurl)
library(httr)
library(twitteR)
library(tm)
library(stringr)
library(plyr)
library(dplyr)
library(wordcloud)
library(ggmap)
library(lubridate)



##Making connection with Twitter
key = "HIDDEN"
secret = "HIDDEN"
atoken = "HIDDEN-DiplOAkWN8HGrOlRuPucCzR0yNYXLQUHVlvcT2Ww"
asecret = "HIDDEN"

setup_twitter_oauth(key,secret,atoken, asecret)


searchTwitter("Samsung")

tweets = searchTwitter("apple+iphone", n=2000, 
                       lang="en", 
                       geocode="34.1,-118.2,150mi")

tweettext = sapply(tweets,
                   function(x)
                     x$getText()
                   )

tweetdate = lapply(tweets, function(x) x$getCreated())
tweetdate = sapply(tweetdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))


isretweet = sapply(tweets,function(x) x$getIsRetweet())

retweetcount = sapply(tweets,function(x) x$getRetweetCount())

favoritecount = sapply(tweets, function(x) x$getFavoriteCount())


tweettext = lapply(tweettext,function(x)
                                iconv(x,
                                      "latin1",
                                      "ASCII",
                                      sub = ""))

tweettext = lapply(tweettext, function(x) gsub("htt.*", "", x))

tweettext = lapply(tweettext, function(x) gsub("#","",x))

tweettext=unlist(tweettext)


pos = readLines("C:/Users/omaro/Documents/Projects/Twitter Sentiment Analysis/opinion-lexicon-English/positive-words.txt")
neg = readLines("C:/Users/omaro/Documents/Projects/Twitter Sentiment Analysis/opinion-lexicon-English/negative-words.txt")
neg2 = c(neg,"bearish","fraud"); tail(neg2)


sentimentfun = function(tweettext,pos,neg, .progress = 'non')
{
  scores = laply(tweettext,
                  function(singletweet,pos,neg)
    {
      singletweet = gsub("[[:punct:]]","",singletweet)
      singletweet = gsub("[[cntrl:]]","",singletweet)
      singletweet = gsub("\\d+","",singletweet)
      
      tryTolower = function(x)
      {
        y=NA
        try_error = tryCatch(tolower(x), error = function(e) e)
        if(!inherits(try_error,"error"))
          y = tolower(x)
        return(y)
      }
      singletweet = sapply(singletweet, tryTolower)
      
      word.list = str_split(singletweet, "\\s+")
      words = unlist(word.list)
      
      pos.matches = match(words, pos)
      neg.matches = match(words, neg)
      
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      score = sum(pos.matches) - sum(neg.matches)
      return(score)
    }, pos, neg, .progress=.progress)
  
  sentiment.df = data.frame(text = tweettext, score=scores)
  return(sentiment.df)
}


scores = sentimentfun(tweettext,pos,neg, .progress='text')



data = as.data.frame(cbind(ttext=tweettext,
                           date=tweetdate,
                           isretweet=isretweet,
                           retweetcount=retweetcount,
                           favoritecount=favoritecount,
                           score = scores$score,
                           product = "Apple iPhone",
                           city="Los Angeles", 
                           country="USA"))


data2 = duplicated(data[,1])

data$duplicate = data2




write.csv(data, file= "C:/Users/omaro/Documents/Projects/Twitter Sentiment Analysis/apple_losangeles.csv")



