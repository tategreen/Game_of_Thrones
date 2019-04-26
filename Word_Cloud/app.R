library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)

data <- read_rds("words.rds")

seasons <<- unique(data$season)

# Using "memoise" to automatically cache the results
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
  
  senti <- data %>%
    unnest_tokens(word, text) %>%
    group_by(season) %>%
    mutate(linenumber = row_number())%>%
    ungroup()

  all_words <- senti %>%
    filter(n > 300) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() 
})

ui <- fluidPage(
  # Application title
  titlePanel("Game of Thrones"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
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
    
    # Show Word Cloud
    mainPanel( 
      tabsetPanel(id = "tabs",
                           tabPanel("About", htmlOutput("about")),
      tabPanel("Word Cloud", plotOutput("plot")),
      tabPanel("Graphs", plotOutput("plot2")))
    )
  )
)


server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
output$plot2 <- renderPlot({
 
  
}) 

output$about <- renderUI({
  str1 <- paste("Game of Thrones")
  str2 <- paste("The plot: ")
  str3 <- paste("") 
  str4 <- paste("")
  
  HTML(paste(h1(str1), p(str2), h1(str3), p(str4)))})}

shinyApp(ui = ui, server = server)
