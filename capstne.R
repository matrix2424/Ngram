# Reading the files

f1= 'en_US.blogs.txt'
con1= file(f1, open='r')
lines1= readLines(con1)

f2= 'en_US.news.txt'
con2= file(f2, open='r')
lines2= readLines(con2)

f3= 'en_US.twitter.txt'
con3= file(f3, open='r')
lines3= readLines(con3)


ns2= length(lines2)
ns1= length(lines1)
ns3= length(lines3)

# Taking a random sample from each file so that the total lines from all files 
# are roughly equal

x1= sample(1:ns1 ,0.03*ns1, replace = F)
x2= sample(1:ns2, 0.3*ns2, replace= F)
x3= sample(1:ns3, 0.01*ns3, replace= F)

train= c(lines1[x1], lines2[x2], lines3[x3])
train = as.character(train)

library(dplyr)
library(stringr)
library(tau)


# Making bigrams

r2 = textcnt(train, split = "[[:space:][:punct:][:digit:]]+", method = "string",
             n = 2L, decreasing= TRUE)

bigrams = data.frame(counts = unclass(r2))
bigrams= data.frame(ngram= rownames(bigrams), count= bigrams$counts)
bigrams= bigrams %>% mutate(base= word(ngram, 1, -2), pred= word(ngram, -1), ngram=NULL)
bigrams= bigrams[c(2,3,1)]
head(bigrams, 10)


# Making trigrams

r3 = textcnt(train, split = "[[:space:][:punct:][:digit:]]+", method = "string",
             n = 3L, decreasing= TRUE)
trigrams = data.frame(counts = unclass(r3), size = nchar(names(r3)))
trigrams= data.frame(ngram= rownames(trigrams), count= trigrams$counts)
trigrams= trigrams %>% mutate(base= word(ngram, 1, -2), pred= word(ngram, -1), ngram=NULL)
trigrams= trigrams[c(2,3,1)]
head(trigrams, 10)


# Making 4-grams

r4 = textcnt(train, split = "[[:space:][:punct:][:digit:]]+", method = "string",
             n = 4L, decreasing= TRUE)
quadgrams = data.frame(counts = unclass(r4), size = nchar(names(r4)))
quadgrams= data.frame(ngram= rownames(quadgrams), count= quadgrams$counts)
quadgrams= quadgrams %>% mutate(base= word(ngram, 1, -2), pred= word(ngram, -1), ngram=NULL)
quadgrams= quadgrams[c(2,3,1)]
head(quadgrams, 10)


# Making 5-grams

r5 = textcnt(train, split = "[[:space:][:punct:][:digit:]]+", method = "string",
             n = 5L, decreasing= TRUE)
fivegrams = data.frame(counts = unclass(r5), size = nchar(names(r5)))
fivegrams= data.frame(ngram= rownames(fivegrams), count= fivegrams$counts)
fivegrams= fivegrams %>% mutate(base= word(ngram, 1, -2), pred= word(ngram, -1), ngram=NULL)
fivegrams= fivegrams[c(2,3,1)]
head(fivegrams, 10)

# Combining all the dataframes

data= rbind(bigrams, trigrams, quadgrams, fivegrams)

library(data.table)
dt= data.table(data)

# Keeping ngrams with count at least 5

dt= dt[dt$count>4]

write.csv(dt, 'ngms1.csv')


# Reading the file

df= read.csv('ngms1.csv')
df=df[c(2,3,4)]
dt= data.table(df)


# Setting key as base and pred

setkey(dt, base, pred)


# Defining a function to predict the next using using png as input

predict_word= function(png){
  
  n_words= sapply(strsplit(png, " "), length)

  
# Taking only the last 4 words of the input for prediction
  
  if (n_words <=4) {png=png}
  
  else { png= word(png, -4,-1)}

# Stupid backoff algorithm was used to predict the next word
# The recommended lambda value of 0.4 was used
  
  den1= as.integer(dt[.(word(png, 1,-2), word(png, -1)), nomatch=0L][,3])
  num1= dt[.(png), nomatch=0L][,2:3]
  num1= num1[order(num1$count, decreasing = T), ][1:5,]
  res1= data.frame(num1) %>% mutate(probability= count/den1, count=NULL)
  colnames(res1)= c('Word', 'Score')
  

# Checking the last 3 words
  
  den2= as.integer(dt[.(word(png, 2,-2), word(png, -1)), nomatch=0L][,3])
  num2= dt[.(word(png, 2,-1)), nomatch=0L][,2:3]
  num2= num2[order(num2$count, decreasing = T), ][1:5,]
  res2= data.frame(num2) %>% mutate(probability= 0.4*count/den2, count=NULL)
  colnames(res2)= c('Word', 'Score')
  
  
# Checking the last 2 words
  
  den3= as.integer(dt[.(word(png, 3,-2), word(png, -1)), nomatch=0L][,3])
  num3= dt[.(word(png, 3,-1)), nomatch=0L][,2:3]
  num3= num3[order(num3$count, decreasing = T), ][1:5,]
  res3= data.frame(num3) %>% mutate(probability= 0.4*0.4*count/den3, count=NULL)
  colnames(res3)= c('Word', 'Score')
  
#  Checking the last word
  
  num4= dt[.(word(png, -1)), nomatch=0L][,2:3]
  num4= num4[order(num4$count, decreasing = T), ][1:5,]
  s= sum(num4$count)
  res4= data.frame(num4) %>% mutate(probability= 0.4*0.4*0.4*count/s, count=NULL)
  colnames(res4)= c('Word', 'Score')
  

# Defining a default dataframe for out of vocabulary words
  
  wds= c('the','and', 'in','for','to')
  scrs= c(0.0000001,0.0000001,0.0000001,0.0000001,0.0000001)
  dflt= data.frame(wds, scrs, stringsAsFactors = F)
  colnames(dflt)= c('Word', 'Score')
  
  
# Combining the results from all backoffs
  
  if (exists('res1')) {res= rbind(res1, res2, res3, res4, dflt)}
  else if (exists('res2') ){ res= rbind(res2, res3, res4, dflt)}
  else if (exists('res3'))  { res= rbind(res3, res4, dflt)}
  else if (exists('res4')) { res= rbind(res4, dflt)}
  
  res= na.omit(res)
  
  
  
# Removing duplicate predictions and keeping the one with higher score
  
  rem= c()
  for (i in 1:nrow(res)){
    for (j in 1:nrow(res)){
      
      if ((res[i,1]==res[j,1]) & (i!=j)){
        m= min(res[i,2],res[j,2])
        r= which(res[,2]==m)
        rem= c(rem,r)
      }
    }
  } 
  
  
  if (is.null(rem)){res= res}
  else{
    n= length(rem)/2
    rem= rem[1:n]
    res= res[-rem,]
  }
  
  
# Displaying top 5 words with scores in descending order
  
  res= res[order(res$Score, decreasing = T),][1:5,]
  row.names(res)= c(1,2,3,4,5)
  res
}

