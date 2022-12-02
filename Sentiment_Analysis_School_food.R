library(dplyr)
library(readxl)
library(readr)        # reads in CSV
library(ggplot2)      # plot library
library(tidyverse) 
# for data manipulation
library(gridExtra)    # multiple plots in 1
library(magick)       # attach dope image for visual
library(scales)       # show the colors
library(ggrepel)      # for graph repel (labels)
library(repr)         # resize graphs
library(hexbin)       # for hive scatter
library(naniar)       # to check for missing data
library(lubridate)    # for date and time
library(tm)
library(wordcloud)    # beautiful wordclouds
library(wordcloud2)
library(tidytext)     # text preprocessing
library(textdata)     # text preprocessing
library(reshape2)
library(knitr)
library(grid)
library(igraph)
library(ggraph)
library(ggsci)
library(devtools)
library(circlize)
library(radarchart)
library(stringr)
library(sjmisc)
library(magick)
library(htmlwidgets)
library(VIM)          # missing values visual
library(colorspace)   # maybe for wordcloud
library(RWeka)
library(textmineR)
library(readtext)
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textplots)

# Загрузка цветовой палитры
options(repr.plot.width=15, repr.plot.height=7)

# Собираем палитру
my_colors <- c("#E3F2FD", "#90CAF9", "#42A5F5", "#1E88E5", "#1565C0", "#0D47A1", '#d90429')

show_col(my_colors, labels = F, borders = NA)

# Собираем способ построения графиков
my_theme <- theme(plot.background = element_rect(fill = "grey98", color = "grey20"),
                  panel.background = element_rect(fill = "grey98"),
                  panel.grid.major = element_line(colour = "grey87"),
                  text = element_text(color = "grey20"),
                  plot.title = element_text(size = 22),
                  plot.subtitle = element_text(size = 17),
                  axis.title = element_text(size = 15),
                  axis.text = element_text(size = 15),
                  legend.box.background = element_rect(color = "grey20", fill = "grey98", size = 0.1),
                  legend.box.margin = margin(t = 3, r = 3, b = 3, l = 3),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 15),
                  strip.text = element_text(size=17))

# Загружаем семантические словари
SenticNet_ru <- read.csv('/Users/sergejromanov/Desktop/Летняя школа/Проект/Семантические словари/senticnet.csv', sep= "\t", header = TRUE)
SenticNet_ru$polarity <- ifelse(SenticNet_ru$POLARITY == 'positive', 'позитивный', 'негативный')
SenticNet_ru <- select(SenticNet_ru, c(-X, -POLARITY ))
names(SenticNet_ru) <- c('word', 'sentiment')
SenticNet_ru$num_polarity <- ifelse(SenticNet_ru$sentiment == 'позитивный', 1, 0)
#SenticNet_ru$stemmed <-  tokens(SenticNet_ru$word)%>%
#  tokens_wordstem(language='ru') %>%
#  tokens_tolower()
names(SenticNet_ru) <- c('word', 'sentiment', 'num_polarity')

write.csv(SenticNet_ru, "SenticNet_ru.csv", row.names = FALSE)

RuSentiLex <- read.csv('/Users/sergejromanov/Desktop/Летняя школа/Проект/Семантические словари/rusentilex_2017.csv', header = FALSE)
names(RuSentiLex) <- c('token','type','token2','sentiment','type')

####------------------------------ Данные ----------------------------------####

#Загрузка данных
school_food_vars <- read.csv('/Users/sergejromanov/Desktop/Летняя школа/Проект/RODKOM+.csv', sep = ';')

school_food <- read_excel('/Users/sergejromanov/Desktop/Летняя школа/Проект/Оценка_организации_бесплатного_горячего_питания_представителями.xlsx')
food_text <- select(school_food, c(20, 35, 36, 37, 38, 39, 40, 41, 42))
names(food_text) <- c('Feedback', 'Region', 'Settlement_type', 
                      'Settlement_name', 'School_type', 'School_number', 
                       'School_INN', 'Parent_committee_grade', 'Parent_committee')

#Проверяем миссинги 
aggr(food_text)
sum(is.na(food_text))

#Фильтруем миссинги по наличию отзывов
food_text <- food_text %>% 
  filter(!is.na(food_text$Feedback)  & (food_text$Feedback != '-' ) 
    & (food_text$Feedback != '.') & (food_text$Feedback != '*'))

dim(food_text)

#Строим распределение по регионам и по типу населённого пункта
options(repr.plot.width=15, repr.plot.height=10)

food_text %>%
  group_by(Region) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  filter(Region != "NA") %>%
  
  ggplot(aes(x = reorder(Region, n), y = n, fill=n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low=my_colors[2], high=my_colors[6], guide="none") +
  geom_label(aes(label=n), size=5, fill="white") +
  labs(title = "Распределение комментариев по регионам") +
  my_theme + theme(axis.text.x = element_blank(),
                   axis.title = element_blank())

food_text %>%
  group_by(Settlement_type) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  filter(Settlement_type != "NA") %>%
  
  ggplot(aes(x = reorder(Settlement_type, n), y = n, fill=n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low=my_colors[2], high=my_colors[6], guide="none") +
  geom_label(aes(label=n), size=5, fill="white") +
  labs(title = "Распределение комментариев по типам населенных пунктов") +
  my_theme + theme(axis.text.x = element_blank(),
                   axis.title = element_blank())


#### ---------------------- Предобработка текста и сети ---------------------------####


food_corpus <- corpus(food_text$Feedback)


food_tokens <- tokens(food_corpus,"word", remove_numbers = TRUE, 
                      remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% 
  tokens_remove(c(stopwords("ru")))

food_dfm <- dfm(food_tokens)
head(dfm_sort(food_dfm, decreasing = TRUE, margin = "both"), n = 12) 
topfeatures(food_dfm)

### Пробуем смотреть контекст использвания слова
kwic(food_tokens, pattern="вопрос")

### Строим сеть по всем н-граммам токена
food_fcm <- food_tokens %>% fcm(context = "window", window = 3)
topfeats <- names(topfeatures(food_fcm, 40))
textplot_network(fcm_select(food_fcm, topfeats))

#### -------------- Предобработка текста по регионам -----------------------####
### Раздел по регионам
Adygea_text <- food_text %>% filter(Region == 'Республика Адыгея')
Khabarovsk_text <- food_text %>% filter(Region == 'Хабаровский край')
Krasnoyarsk_text <- food_text %>% filter(Region == 'Красноярский край')
Tatarstan_text <- food_text %>% filter(Region == 'Республика Татарстан')
Mordovia_text <- food_text %>% filter(Region == 'Республика Мордовия')

### Корпусы по регионам

Adygea_corpus <- corpus(Adygea_text$Feedback)
Khabarovsk_corpus <- corpus(Khabarovsk_text$Feedback)
Krasnoyarsk_corpus <- corpus(Krasnoyarsk_text$Feedback)
Tatarstan_corpus <- corpus(Tatarstan_text$Feedback)
Mordovia_corpus <- corpus(Mordovia_text$Feedback)

### Токенизация по регионам
Adygea_tokens <- tokens(Adygea_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% tokens_remove(c(stopwords("ru")))
Khabarovsk_tokens <- tokens(Khabarovsk_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% tokens_remove(c(stopwords("ru")))
Krasnoyarsk_tokens <- tokens(Krasnoyarsk_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% tokens_remove(c(stopwords("ru")))
Tatarstan_tokens <- tokens(Tatarstan_corpus , remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% tokens_remove(c(stopwords("ru")))
Mordovia_tokens <- tokens(Mordovia_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% tokens_remove(c(stopwords("ru")))

##### Создаем датафреймы по регионам
Adygea_dfm <- dfm(Adygea_tokens)
Khabarovsk_dfm <- dfm(Khabarovsk_tokens)
Krasnoyarsk_dfm <- dfm(Krasnoyarsk_tokens)
Tatarstan_dfm <- dfm(Tatarstan_tokens)
Mordovia_dfm <- dfm(Mordovia_tokens)

##### Создаем матрициы соиспользования слов по регионам
Adygea_fcm <- Adygea_tokens %>% fcm(context = 'window', window = 3)
Khabarovsk_fcm <- Khabarovsk_tokens %>% fcm(context = 'window', window = 3)
Krasnoyarsk_fcm <- Krasnoyarsk_tokens %>% fcm(context = 'window', window = 3)
Tatarstan_fcm <- Tatarstan_tokens %>% fcm(context = 'window', window = 3)
Mordovia_fcm <- Mordovia_tokens %>% fcm(context = 'window', window = 3)

#### Cобираем топ 40 биграмм по регионам
Adygea_topfeats <-names(topfeatures(Adygea_fcm, 40))
Khabarovsk_topfeats <-names(topfeatures(Khabarovsk_fcm , 40))
Krasnoyarsk_topfeats <- names(topfeatures(Krasnoyarsk_fcm, 40))
Tatarstan_topfeats <- names(topfeatures(Tatarstan_fcm, 40))
Mordovia_topfeats <-names(topfeatures(Mordovia_fcm, 40))


#### Стоим графики по 6 регионам 
textplot_network(fcm_select(Adygea_fcm, Adygea_topfeats))
textplot_network(fcm_select(Khabarovsk_fcm, Khabarovsk_topfeats))
textplot_network(fcm_select(Krasnoyarsk_fcm, Krasnoyarsk_topfeats))
textplot_network(fcm_select(Tatarstan_fcm, Tatarstan_topfeats))
textplot_network(fcm_select(Mordovia_fcm, Mordovia_topfeats)) 

####--------------------- The Chord Diagram  --------------------------####

unnest_food_tokens <- food_text %>% 
  mutate(text = as.character(food_text$Feedback))%>% 
  unnest_tokens(word, Feedback) %>% 
  
  

 
options(repr.plot.width=15, repr.plot.height=9)

total_SenticNet <- unnest_food_tokens %>% 
  inner_join(SenticNet_ru, by="word") %>%
  count(Region) %>% 
  group_by(Region) %>%
  summarise(total_tweets = sum(n), .groups = "drop_last")


to_plot <- unnest_food_tokens %>% 
  # Фильтруем по наличию токенов в лексиконе 'SenticNet_ru' 
  inner_join(SenticNet_ru, by="word") %>%
  
  # Суммируем кол-во слов, группируя по региону и по тональности
  count(sentiment, Region) %>% 
  group_by(Region, sentiment) %>% 
  summarise(sentiment_sum = sum(n), .groups = "drop_last") %>% 
  inner_join(total_SenticNet, by="Region") %>% 
  mutate(sentiment_perc = sentiment_sum/total_tweets) %>% 
  select(Region, sentiment, sentiment_perc)

# The Chord Diagram  
circos.clear()
circos.par(gap.after = c(rep(2, length(unique(to_plot[[1]])) - 1), 15,
                         rep(2, length(unique(to_plot[[2]])) - 1), 15), gap.degree=2)

myColors = c("Республика Адыгея" = '#1E88E5', "Хабаровский край" = '#E3F2FD', "Красноярский край" = "#1565C0", "Краснодарский край" = "#0D47A1", 
             "Республика Татарстан" = '#90CAF9', "Республика Мордовия" = '#42A5F5',
             "позитивный" = "#6a994e", "негативный" = "#db504a")

chordDiagram(to_plot, grid.col = myColors, transparency = 0.2, annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.03, 0.06))

title("Отношение между тональностью отзыва и регионом")

####--------------------------- Облака слов --------------------------------####
dfsw <- unlist(stopwords('ru'))
dfsw <- as.data.frame(dfsw)

dr <- anti_join(unnest_food_tokens, dfsw, by = c('word'='dfsw'))

unnest_food_tokens$word %>% 
  anti_join(stopwords('ru'), "word") 

options(repr.plot.width=15, repr.plot.height=15)

unnest_food_tokens %>% 
  
  inner_join(SenticNet_ru, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  comparison.cloud(colors=my_colors[c(5, 1)], max.words = 400, title.size = 2,
                   scale = c(3,.5))

dr %>% 
  inner_join(SenticNet_ru, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  comparison.cloud(colors=my_colors[c(7, 6)], max.words = 400, title.size = 2,
                   scale = c(3,.5))


#stop_words <-  read_csv("/Users/panderlogus/Desktop/школа байкл/stopwords_ru.csv")
tidy_food <- tibble(text = food_text$Feedback)
tokens_food <- tidy_food %>% unnest_tokens(word, text)
tokens_food <- tokens_food %>% anti_join(stop_words)
word_freq <- tokens_food %>% count(word) %>% arrange(desc(n()))
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 10, max.words = 100)


####----------------------------- TF-IDF -----------------------------------####
food_tfidf <- dfm_tfidf(food_dfm)

####------------ Формировка зависимой тоннальной переменной ----------------####

total_SenticNet_word <- unnest_food_tokens %>% 
  inner_join(SenticNet_ru, by="word") %>%
  count(text) %>% 
  group_by(text) %>%
  summarise(total_tweets = sum(n), .groups = "drop_last")


sentiment_variable <- z %>% 
  # Фильтруем по наличию токенов в лексиконе 'SenticNet_ru' 
  inner_join(SenticNet_ru, by="word") %>%
  # Суммируем кол-во слов, группируя по региону и по тональности
  count(sentiment, text) %>% 
  group_by(text, sentiment) %>% 
  summarise(sentiment_sum = sum(n), .groups = "drop_last") %>% 
  inner_join(total_SenticNet_word, by="text") %>% 
  mutate(sentiment_perc = sentiment_sum/total_tweets) %>% 
  select(text, sentiment, sentiment_perc)


write.csv(sentiment_variable, file='sentiment_variable.csv',  row.names = FALSE)

####----------------------------- Регрессия --------------------------------####

