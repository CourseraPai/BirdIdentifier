---
title: "Lustre"
output: html_document
---



library(shiny)
library(gutenbergr)
library(tidyverse)
library(tidytext)
library(wordcloud)


ui <- fluidPage(
  titlePanel("Top 50 Words"),
  sidebarLayout(
    sidebarPanel(radioButtons("Name", "Select a Novel:",
                 list("Pride and Prejudice" = "Pride and Prejudice",
                      "Emma" = "Emma",
                      "Howards End" = "Howards End",
                      "Far from the madding crowd" = "Far from the madding crowd",
                      "Great Expectations" =  "Great Expectations"))),
    mainPanel(plotOutput("wordcloud"))
  )
)
    
    
    
server=function(input,output){
  
 output$wordcloud<-renderPlot({
   id<-ifelse(input$Name=="Pride and Prejudice",1342,
              ifelse(input$Name=="Emma",158,
                     ifelse(input$Name=="Howards End",2891,
                            ifelse(input$Name=="Far from the madding crowd",27,
                                   ifelse(input$Name=="Great Expectations",1400,1342)))))
   
   book<-gutenberg_download(id)
   
   words<-book %>%
     unnest_tokens(word,text)
   
   word_count<-words %>%
     anti_join(stop_words,by="word") %>%
     count(word,sort=TRUE)
   
   with(word_count,wordcloud(word,n,max=50))
   
 }) 
  
}
shinyApp(ui=ui,server=server)


```

