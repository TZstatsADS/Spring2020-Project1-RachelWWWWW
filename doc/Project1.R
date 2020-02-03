load('../data/lyrics.RData') 

load('../output/processed_lyrics.RData') 

library(stringr)
data(dt_lyrics)

load('../output/processed_lyrics.RData')


dt_rock = dt_lyrics %>% 
  filter(genre == "Rock")
lyrics_rock = dt_rock %>% pull(stemmedwords) %>% paste(collapse = " ") %>% 
  strsplit(" ") %>% unlist()
words_rock = tibble(words = lyrics_rock) %>% 
  group_by(words) %>% count() %>%
  arrange(desc(n))


dt_hiphop = dt_lyrics %>% 
  filter(genre == "Hip-Hop")
lyrics_hiphop = dt_hiphop %>% pull(stemmedwords) %>% paste(collapse = " ") %>% 
  strsplit(" ") %>% unlist()
words_hiphop = tibble(words = lyrics_hiphop) %>% 
  group_by(words) %>% count() %>%
  arrange(desc(n))

wordcloud(words_rock$words, words_rock$n,
          scale=c(5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))

wordcloud(words_hiphop$words, words_hiphop$n,
          scale=c(5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))

library(shiny)
library(wordcloud)
shinyApp(
  ui = fluidPage(
    fluidRow(style = "padding-bottom: 20px;",
             column(4, selectInput('Rock n Roll', 'Rock n Roll',
                                   speeches,
                                   selected=speeches[5])),
             column(4, selectInput('speech2', 'Speech 2', speeches,
                                   selected=speeches[9])),
             column(4, sliderInput('nwords', 'Number of words', 3,
                                   min = 20, max = 200, value=100, step = 20))
    ),
    fluidRow(
      plotOutput('wordclouds', height = "400px")
    )
  ),
  
  server = function(input, output, session) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
      list(dtm.term1=ff.dtm$term[ff.dtm$document==as.character(input$speech1)],
           dtm.count1=ff.dtm$count[ff.dtm$document==as.character(input$speech1)],
           dtm.term2=ff.dtm$term[ff.dtm$document==as.character(input$speech2)],
           dtm.count2=ff.dtm$count[ff.dtm$document==as.character(input$speech2)])
    })
    
    output$wordclouds <- renderPlot(height = 400, {
      par(mfrow=c(1,2), mar = c(0, 0, 3, 0))
      wordcloud(selectedData()$dtm.term1, 
                selectedData()$dtm.count1,
                scale=c(4,0.5),
                max.words=input$nwords,
                min.freq=1,
                random.order=FALSE,
                rot.per=0,
                use.r.layout=FALSE,
                random.color=FALSE,
                colors=brewer.pal(10,"Blues"), 
                main=input$speech1)
      wordcloud(selectedData()$dtm.term2, 
                selectedData()$dtm.count2,
                scale=c(4,0.5),
                max.words=input$nwords,
                min.freq=1,
                random.order=FALSE,
                rot.per=0,
                use.r.layout=FALSE,
                random.color=FALSE,
                colors=brewer.pal(10,"Blues"), 
                main=input$speech2)
    })
  },
  
  options = list(height = 600)
)



library("syuzhet")
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)

dt_rock_emo = dt_rock %>% 
  mutate(emotions=map(stemmedwords,get_nrc_sentiment)) %>% 
  unnest(emotions)

dt_rock_emo_plot = dt_rock_emo %>%  
  mutate(x = seq_along(id)) %>% 
  select(x, anger:trust) %>% 
  pivot_longer(anger:trust) %>% 
  group_by(x) %>% 
  top_n(1, value) %>% 
  mutate(color = map(name, ~switch (.x,
                                    anticipation = "light grey", 
                                    joy = "red", 
                                    surprise = "orange", 
                                    trust = "darkgoldenrod1", 
                                    anger = "red2", 
                                    disgust = "chartreuse3", 
                                    fear = "blueviolet", 
                                    sadness = "dodgerblue3"))
  ) %>% 
  unnest(color)

plot(dt_rock_emo_plot$x, dt_rock_emo_plot$value, 
     col = dt_rock_emo_plot$color, type="h", main = "Rock", xlab = "", ylab = "", xlim = c(1, 9000), ylim = c(0,30))


dt_hiphop_emo = dt_hiphop %>% 
  mutate(emotions=map(stemmedwords,get_nrc_sentiment)) %>% 
  unnest(emotions)

dt_hiphop_emo_plot = dt_hiphop_emo %>% 
  mutate(x = seq_along(id)) %>% 
  select(x, anger:trust) %>% 
  pivot_longer(anger:trust) %>% 
  group_by(x) %>% 
  top_n(1, value) %>% 
  mutate(color = map(name, ~switch (.x,
                                    anticipation = "light grey", 
                                    joy = "red", 
                                    surprise = "orange", 
                                    trust = "darkgoldenrod1", 
                                    anger = "red2", 
                                    disgust = "chartreuse3", 
                                    fear = "blueviolet", 
                                    sadness = "dodgerblue3"))
  ) %>% 
  unnest(color)

plot(dt_hiphop_emo_plot$x, dt_hiphop_emo_plot$value, 
     col = dt_hiphop_emo_plot$color, type="h")


save(dt_rock_emo, file="../output/dt_rock_emotions.RData")
save(dt_hiphop_emo, file="../output/dt_hiphop_emotions.RData")


library(topicmodels)

dt_rock = dt_lyrics %>% 
  filter(genre == "Rock")

rock_lyrics = dt_rock %>% 
  
  
  lyrics_rock = dt_rock %>% pull(stemmedwords) %>% paste(collapse = " ") %>% 
  strsplit(" ") %>% unlist()

words_rock = tibble(words = lyrics_rock) %>% 
  group_by(words) %>% count() %>%
  arrange(desc(n))

words_rock.1 = words_rock %>% mutate(document = 1)
words_hiphop.1 = words_hiphop %>% mutate(document = 2)

word_count = rbind(words_rock.1, words_hiphop.1)

library(tidytext)
chapters_dtm <- word_count %>%
  cast_dtm(document, words, n)
chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 6, control = list(seed = 1234))
chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

library(qdap)
dt_rock %>% head(3) %>% mutate(sentences=sent_detect(lyrics,
                                                     endmarks = c("?", ".", "!", "|",";", "\n")))





