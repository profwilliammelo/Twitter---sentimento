
library(rtweet)
library(DT)
library(tidyverse)


# rtweet::auth_setup_default()

# lula <- rtweet::search_tweets("Lula", n = 10000)

# lula %>% select(created_at, id, id_str, full_text, truncated, source, favorited_by, text, retweet_count) %>% 
#  rio::export(file = "lula_10ktweets03012023_23h", format = "csv")

lula <- read.csv("lula_10ktweets03012023_23h.csv")

#install.packages("syuzhet")

palavras <- syuzhet::get_tokens(lula$full_text)

#sentimentos_lula <- syuzhet::get_nrc_sentiment(palavras, language = "portuguese")

#rio::export(sentimentos_lula, file = "sentimentos_lula", format = "csv")

sentimentos_lula <- read.csv("sentimentos_lula.csv")

#sentimentos_lula %>% mutate(palavra = palavras) %>% View()



sentimentos_lula_pivotado <- sentimentos_lula %>% 
  summarise(across(.cols = everything(), .fns = ~ sum(.x))) %>% 
  pivot_longer(cols = everything())

sentimentos_lula_pivotado %>%
  mutate(name = factor(
    name,
    levels = c(
      "anger",
      "disgust",
      "fear",
      "sadness",
      "anticipation",
      "joy",
      "surprise",
      "trust",
      "negative",
      "positive"
    ),
    labels = c(
      "raiva",
      "aversão",
      "medo",
      "tristeza",
      "antecipação/ansiedade",
      "alegria",
      "surpresa",
      "confiança",
      "negativo",
      "positivo"
    )
  )) %>%
  ggplot(aes(x = name, y = value, fill = value)) + geom_col(col = "black") +
  xlab("Nome do setimento") + ylab("Quantidade de palavras") +
  labs(fill = "Quantidadede de palavras",
       title = "Análise de sentimento com relação à palavra 'Lula', no Twitter",
       subtitle = "Últimos 10000 tweets: 03/01/2023, 23h",
       caption = "William Melo | Educação com Evidências") + coord_flip() + ggthemes::theme_calc(base_size = 22)


#install.packages('translateR')
 
 # Nuvem de palavras
 
 # Nuvem de palavras de sugestoes do publico ----------
 
 library(RColorBrewer)
 library(wordcloud2)
 library(tm)
 
 # Create a vector containing only the text
 
 texto <- lula$full_text
 
 # Create a corpus  
 
 docs <- Corpus(VectorSource(texto))
 
 # Limpando
 
 docs <- docs %>% tm_map()
   tm_map(removeNumbers) %>%
   tm_map(removePunctuation) %>%
   tm_map(stripWhitespace)
 docs <- tm_map(docs, content_transformer(tolower))
 docs <- tm_map(docs, removeWords, stopwords("portuguese"))
 
 # Contabilizando as palavras 
 dtm <- TermDocumentMatrix(docs) 
 matrix <- as.matrix(dtm) 
 words <- sort(rowSums(matrix),decreasing=TRUE) 
 df <- data.frame(word = names(words), freq=words)
 
 # Criando a nuvem 
 
 nuvem <- wordcloud2(data=df, size=1.6, color='random-dark')
 
 
 




