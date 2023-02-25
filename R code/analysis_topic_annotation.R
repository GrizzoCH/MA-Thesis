#### Imports ####
require(readtext)
require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(openxlsx)
require(tidyverse)
require(ggplot2)
require(reshape2)

#### Datenkombination Topic Values und Originale Textdaten ####

myData <- read.xlsx("Data_Pro4.xlsx") |> rename(doc_id = id)
myData$pubtime <- as.Date(myData$pubtime, origin = "1899-12-30")
topicsData <- read.xlsx("TPD_L_NPE_50_Optim.xlsx") |> rename(doc_id = Text.ID)

fullData <- inner_join(myData, topicsData, by = "doc_id")

write.xlsx(fullData, 'fullData.xlsx') # Export kombinierte Daten

#### Analysis Topic over Time ####
fullData <- read.xlsx("fullData.xlsx")
fullData$pubtime <- as.Date(fullData$pubtime, origin = "1899-12-30")

Topics_Date <- aggregate(fullData[,7:56], by=list(Category=fullData$pubtime), FUN=sum) |> rename(pubtime = Category)

Publics_Date <- fullData |> group_by(pubtime) |> summarise(count = n())

combined_Data <- left_join(Topics_Date, Publics_Date, by = "pubtime")

temp <- mapply('/', combined_Data[2:51], combined_Data[52])
temp <- as.data.frame(temp)

Publics_Date <- bind_cols(Publics_Date, temp)


df3 <- Publics_Date |> select(!count) |> melt(, id.vars = 'pubtime')

ggplot(df3, mapping = aes(x = pubtime, y=value, color = variable)) +
  geom_line() +
  scale_x_date(date_labels = '%b') +
  xlab("Publikationsdatum") +
  ylab('Anteil') +
  guides(color = "none") +
  facet_wrap(~variable)

write.xlsx(df3, 'topicsPday.xlsx')

df1 <- Publics_Date |> select(pubtime, Topic.9, Topic.14, Topic.19, Topic.23) |> melt(, id.vars = 'pubtime')

topic.labs <- c("Impfstoffe", "Kurzarbeit/Homeoffice", "US-Wahlen", "Skigebiete / Wallis")
names(topic.labs) <- c('Topic.9', 'Topic.14', 'Topic.19', 'Topic.23')

ggplot(df1, mapping = aes(x = pubtime, y = value, color = variable)) +
  geom_line() +
  scale_x_date(date_labels = '%b') +
  xlab("Publikationsdatum") +
  ylab('Anteil') +
  facet_wrap(~variable, labeller = labeller(variable = topic.labs))+
  guides(color = "none") +
  ggtitle("Topics über Zeit")

#### Top 10 Pro Topic ####
parsedData <- readRDS('parsedData.rda') #Loading spaCy parsed Data

qCorpus <- corpus(fullData, text_field = "FullText")

print(qCorpus)

qCorp_1 <- corpus_subset(qCorpus, Topic.1 > 0.25)

qToks_1 <- tokens(qCorp_1)

kwic_qToks_1_berset <- kwic(qToks_1, pattern = phrase("Alain Berset"), window = 10)

qCorp_3 <- corpus_subset(qCorpus, Topic.3 > 0.25)

qToks_3 <- tokens(qCorp_3)

kwic_qToks_3_berset <- kwic(qToks_3, pattern = c("fc", "fcz", "yb", "league"), window = 10)

qCorp_18 <- corpus_subset(qCorpus, Topic.18 > 0.25)

qToks_18 <- tokens(qCorp_18)

kwic_qToks_18_berset <- kwic(qToks_18, pattern = c("swiss", "lufthansa"), window = 10)


qCorp_42 <- corpus_subset(qCorpus, Topic.41 > 0.25)

qToks_42 <- tokens(qCorp_42)

kwic_qToks_42_berset <- kwic(qToks_42, pattern = c("fcb"), window = 10)

head(qToks_42)

qCorp_19 <- corpus_subset(qCorpus, Topic.19 > 0.10)

qToks_19 <- tokens(qCorp_19)

kwic_qToks_19_berset <- kwic(qToks_19, pattern = c("Trump", "Biden"), window = 10)
kwic_qToks_19_berset2 <- kwic(qCorpus, pattern = c("Trump", "Biden"), window = 10)

head(qToks_42)

dfmat1 <- dfm(corpus_subset(qCorp_1, Topic.19 >= 0.25),
              remove = stopwords("de"), remove_punct = TRUE) %>%
dfm_trim(min_termfreq = 3)

textplot_wordcloud(dfmat1)

#### Temp Code ####
temp <- fullData |> filter(Topic.9 > 0.25) |> select(doc_id)
selection <- parsedData |> filter(doc_id %in% temp$doc_id)

t9tokens <- as.tokens(selection, remove_punct = TRUE)

output <- t9tokens |> tokens_select(pattern = c("Impfstoff"))

dfmat_t9tokens <- dfm(t9tokens) |> dfm_remove(pattern = stopwords("de"))
tstat_freq_t9 <- textstat_keyness(dfmat_t9tokens)
head(tstat_freq_t9, 20)

textplot_keyness(tstat_freq_t9)

dfmat_t9tokens %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()