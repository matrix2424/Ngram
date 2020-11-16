#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    align='center',
    titlePanel("Word Predictor"),
    br(),

        # Show a plot of the generated distribution
        mainPanel(
            align= 'center',
            h5('This app was made as part of the Data Science Specialization Capstone project.
               The data was a collection of tweets, blogs and news on US sites. Due to memory
               constraints, only around 5 percent of the data could be used.'),
            
            h5
               ('The n-grams considered for the model
               were bigrams, trigrams, fourgrams and fivegrams. For efficiency, those n-grams
               having count less than 5 were discarded.To deal with words not in the data and wrongly spelled or meaningless exressions,
                   a default prediciton has been made.'),
            
            
            h5('In the box type at least one word, do not hit space after the last word of your 
            phrase.
               Please do not type any punctuation marks.'),
            
            br(),
            textInput("caption", "Enter phrase", ''),
            br(),
            h4('Predicted word'),
            span(textOutput('nxt'), style='color:magenta; font-style:bold; font-size:24px'),
            
            br(),
            h4('Probable words with scores using Stupid Backoff', align='center'),
            verbatimTextOutput("value")
    )
)
)