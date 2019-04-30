library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)

data <- read_rds("words.rds")
allwords <- read_rds("all_words.rds")
top_10 <- read_rds("top_10.rds")

## It is important to load in my read_rds s that i made previously 
##from manipulating previous data sets into what I want to work with 
## for graphics to come

seasons <<- unique(data$season)

##Here I create a season column so that I can pick within the wordcloud 
## which season I want to look at and therefore can 

getTermMatrix <- memoise(function(season) {
  
  if (!(season %in% seasons))
    stop("Unknown season")
  
  words <- data$text[data$season == season]
  
  myCorpus = Corpus(VectorSource(words))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
  
})

##I used code from https://shiny.rstudio.com/gallery/word-cloud.html
##to figure out how to make my word cloud code and I am still 
## researching what each function does exactly and how each 
## line of code means and how it effects my shiny app for my Game
## of Thrones analysis


ui <- fluidPage(
  titlePanel("Game of Thrones"),
  
  ## This gives my app a title which I choose to be
  ## Game of Thrones
  
  sidebarLayout(
    
    # This is more code that I got from  
    ## https://shiny.rstudio.com/gallery/word-cloud.html
    ## Which creates the slider where it is possible to 
    ## choose which season you want to look at within 
    ## my wordcloud piece of the final project which is a really
    ## cool function and is used with the the seasons which
    ## I previously made in the function above
    
    sidebarPanel(
      helpText("Word cloud of the most used words."),
      selectInput("selection", "Choose a Season:",
                  choices = seasons),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    ## The sidebarPanel is used to label what we want the 
    ## sidebar to show and the different way I want to show 
    ## my word cloud
    
    mainPanel( 
      tabsetPanel(id = "tabs",
      tabPanel("Word Cloud", plotOutput("plot")),
      tabPanel("Graphs", plotOutput("allwords"), plotOutput("top_10")),
      tabPanel("About", htmlOutput("about")))
    )
  )
)

## The main panel function is used in order to create tabs so that 
## I could put my word cloud on one page, my about the app on 
## a separate page and then another page for the graphs I created

server <- function(input, output, session) {
  terms <- reactive({
    
 ## this makes my code reactive and so it can update based on season 
   
     input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        
        getTermMatrix(input$selection)
      })
    })
  })
  
  ## This is more code pulled from the rstudio  
  ## https://shiny.rstudio.com/gallery/word-cloud.html
  ## which I am going to look more into 
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "RdGy"))
  })
output$allwords <- renderPlot({
 allwords
})
output$top_10 <- renderPlot({
    top_10
})

## Here I put my different already manipulated and created
## graphics into the output function to send to the app
## the allwords and top 10 data frames are those I've 
## created in rscripts already sent to my github and 
## then loaded in through a read_rds

output$about <- renderUI({
  str1 <- paste("Game of Thrones")
  str2 <- paste("Based off of George R.R. Martin's best selling book series.  The hit HBO series, Game of Thrones is in the peak of it's 8th and final season.  As conspiricy theories progress and fan fiction dominates all media fronts, I wanted to take a deeper look into the script of the past 7 seasons and do a deeper analysis for myself.   ")
  str3 <- paste("Things to note:") 
  str4 <- paste("An interesting thing to note within my analysis is in the sentiment analysis how stark is looked at in the negative and I kept it because I thought it was an interesting factor.  Because in the series stark is actually a name and not used in a negative way, it is interesting that the function labeled it as negative.")
  str5 <- paste("Contact Information")
  str6 <- paste("Github Link: https://github.com/tategreen/Game_of_Thrones")
  str7 <- paste("Source: Kaggle.com")
  str8 <- paste("Final Project by Tate Green")
    
  ## within the output of my about tab these are the different lines
  ## I want put in in their specific orders I have a title, then a 
  ## short explanation of why I chose to look into Game of Thrones
  ##Then some things to look out for within my app, then I add
  ## my contact information which shows my name, source which is
  ## Kaggle.com and also my direct github link where all my code can 
  ## be found. I do all this by assigning this text to different
  ## strings and using the paste function 
  
  HTML(paste(h1(str1), p(str2), h1(str3), p(str4), h1(str5), h1(str6), h1(str7), h1(str8)))})}

## Now I use paste again to decide what size and where I want 
## each line of text to be placed within my about page
## this is important to how I want my about page to look 
## However I might change the sizes of each line and would do
## so by changing the p and h1s around.

shinyApp(ui = ui, server = server)

##Here I link the ui and the server before sending to the web