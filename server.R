#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    library(data.table)
    library(dplyr)
    library(stringr)
    library(tau)
    
    df= read.csv('ngms1.csv')
    df=df[c(2,3,4)]
    dt= data.table(df)
    
    setkey(dt, base, pred)
    
    predict_word= function(png){
        
        n_words= sapply(strsplit(png, " "), length)
        
        if (n_words <=4){ png=png}
        
        else { png= word(png, -4,-1)}
        
        
        den1= as.integer(dt[.(word(png, 1,-2), word(png, -1)), nomatch=0L][,3])
        num1= dt[.(png), nomatch=0L][,2:3]
        num1= num1[order(num1$count, decreasing = T), ][1:5,]
        res1= data.frame(num1) %>% mutate(probability= count/den1, count=NULL)
        colnames(res1)= c('Word', 'Score')
        
        
        den2= as.integer(dt[.(word(png, 2,-2), word(png, -1)), nomatch=0L][,3])
        num2= dt[.(word(png, 2,-1)), nomatch=0L][,2:3]
        num2= num2[order(num2$count, decreasing = T), ][1:5,]
        res2= data.frame(num2) %>% mutate(probability= 0.4*count/den2, count=NULL)
        colnames(res2)= c('Word', 'Score')
        
        
        den3= as.integer(dt[.(word(png, 3,-2), word(png, -1)), nomatch=0L][,3])
        num3= dt[.(word(png, 3,-1)), nomatch=0L][,2:3]
        num3= num3[order(num3$count, decreasing = T), ][1:5,]
        res3= data.frame(num3) %>% mutate(probability= 0.4*0.4*count/den3, count=NULL)
        colnames(res3)= c('Word', 'Score')
        
        
        num4= dt[.(word(png, -1)), nomatch=0L][,2:3]
        num4= num4[order(num4$count, decreasing = T), ][1:5,]
        s= sum(num4$count)
        res4= data.frame(num4) %>% mutate(probability= 0.4*0.4*0.4*count/s, count=NULL)
        colnames(res4)= c('Word', 'Score')
        
        
        wds= c('the','and', 'in','for','to')
        scrs= c(0.0000001,0.0000001,0.0000001,0.0000001,0.0000001)
        dflt= data.frame(wds, scrs, stringsAsFactors = F)
        colnames(dflt)= c('Word', 'Score')
        
        
        
        if (exists('res1')) {res= rbind(res1, res2, res3, res4, dflt)}
        else if (exists('res2') ){ res= rbind(res2, res3, res4, dflt)}
        else if (exists('res3'))  { res= rbind(res3, res4, dflt)}
        else if (exists('res4')) { res= rbind(res4, dflt)}
        

        
        res= na.omit(res)
        
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
    
        res= res[order(res$Score, decreasing = T),][1:5,]
        row.names(res)= c(1,2,3,4,5)
        res
    }
    
    
    word_predictor= reactive({
        
        predict_word(input$caption)
    })
    
    nxt_word= reactive({
        
        as.character(predict_word(input$caption)[1,1])
    })

    output$value <- renderPrint({ 
        
        word_predictor() })
    
    
    output$nxt <- renderText({ 
        
        nxt_word()})
    })


