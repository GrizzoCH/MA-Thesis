#### Imports ####
require(readtext)
require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(openxlsx)
require(tidyverse)
require(ggplot2)

#### Import POS Tagged Data
myData <- readRDS('parsedData.rda')

dateData <- read.xlsx('Data_Pro4.xlsx') |> rename(doc_id = id)
dateData$pubtime <- as.Date(dateData$pubtime, origin = "1899-12-30")
dateData$KW <- as.integer(strftime(dateData$pubtime, format = "%V"))

dateData <- dateData |> select(doc_id, pubtime, KW)
dateData$doc_id <- as.character(dateData$doc_id)


#### Funktionen ####
## Sub-Korpus Filterung durch Keywords
subcorpus <- function(keywords){
  temp_txt_list <- myData |> select(doc_id, lemma, token) |> filter(myData$lemma %in% keywords) |> select(doc_id) |> unique()
  return(myData |> filter(myData$doc_id %in% temp_txt_list$doc_id))
}

## 
txt_list <- function(keywords){
  temp <- myData |> select(doc_id, lemma, token) |> filter(myData$lemma %in% keywords) |> select(doc_id) |> unique() |> left_join(dateData)
  temp$Topic <- deparse(substitute(keywords))
  return(temp)
}

## Analyse Akteur + Verb nGramme
analysis_verb <- function(akteur, corpus_selection){
  temp <- tokens_ngrams(corpus_selection, n = 2)
  temp <- tokens_select(temp, pattern = akteur) |> tokens_select(pattern = c('*verb*')) 
  
  temp2 <- as.data.frame(topfeatures(dfm(temp), n = 150))
  temp2$collocation <- row.names(temp2)
  return(temp2)
}

## Analyse Keyword-in-Context Verb Sein
analysis_Sein_KWIC <- function(corpus_selection){
  temp <- kwic(corpus_selection, pattern = c("ist*", "sind*", "bin*", "bist*", "seid*"), window = 20)
  return(temp)
}


#### Topic Impfungen #9
Impfungen <- c("Impfstoff", "Moderna", "Pfizer", "Biontech", "Impfung")
txt_ids_Impfungen <- txt_list(Impfungen)
txt_Impfungen <- subcorpus(Impfungen)
qCorp_Impfungen <- as.tokens(txt_Impfungen, include_pos = 'pos')
qCorp_Impfungen_lemma <- as.tokens(txt_Impfungen, include_pos = 'pos', use_lemma = T)

#### Topic Spitäler
Spital <- c("Spital", "Intensivstation", "Patient", "Patientin", "Patienten",  "IPS")
txt_ids_Spital <- txt_list(Spital)
txt_Spital <- subcorpus(Spital)
qCorp_Spital <- as.tokens(txt_Spital, include_pos = 'pos')
qCorp_Spital_lemma <- as.tokens(txt_Spital, include_pos = 'pos', use_lemma = T)

#### Topic Masken # 
Masken <- c("Schutzmasken", "Hygienemasken", "Masken", "FFP")
txt_ids_Masken <- txt_list(Masken)
txt_Masken <- subcorpus(Masken)
qCorp_Masken <- as.tokens(txt_Masken, include_pos = 'pos')
qCorp_Masken_lemma <- as.tokens(txt_Masken, include_pos = 'pos', use_lemma = T)

#### Topic Ski Wallis #23
Ski_Wallis <- c("Skigebiete", "Wallis", "Skigebieten")
txt_ids_Ski_Wallis <- txt_list(Ski_Wallis)
txt_Ski_Wallis <- subcorpus(Ski_Wallis)
qCorp_Ski_Wallis <- as.tokens(txt_Ski_Wallis, include_pos = 'pos')
qCorp_Ski_Wallis_lemma <- as.tokens(txt_Ski_Wallis, include_pos = 'pos', use_lemma = T)

#### Topic US-Wahlen
US_Wahl <- c("Biden", "Trump", "Wahl")
txt_ids_US_Wahl <- txt_list(US_Wahl)
txt_US_Wahl <- subcorpus(US_Wahl)
qCorp_US_Wahl <- as.tokens(txt_US_Wahl, include_pos = 'pos')


#### Analysen
berset_Impfungen_lemma <- analysis_verb('berset/propn*', qCorp_Impfungen_lemma)
berset_Ski_Wallis_lemma <- analysis_verb('berset/propn*', qCorp_Ski_Wallis_lemma)
berset_Masken_lemma <- analysis_verb('berset/propn*', qCorp_Masken_lemma)
berset_Spital_lemma <- analysis_verb('berset/propn*', qCorp_Spital_lemma)

write.xlsx(berset_Impfungen_lemma, "berset_Impfungen_lemma.xlsx")
write.xlsx(berset_Ski_Wallis_lemma, "berset_Ski_Wallis_lemma.xlsx")
write.xlsx(berset_Masken_lemma, "berset_Masken_lemma.xlsx")
write.xlsx(berset_Spital_lemma, "berset_Spital_lemma.xlsx")

bundesrat_Impfungen <- analysis_verb('bundesrat/noun*', qCorp_Impfungen_lemma)
bundesrat_Ski_Wallis <- analysis_verb('bundesrat/noun*', qCorp_Ski_Wallis_lemma)
bundesrat_Masken <- analysis_verb('bundesrat/noun*', qCorp_Masken_lemma)
bundesrat_Spital <- analysis_verb('bundesrat/noun*', qCorp_Spital_lemma)

write.xlsx(bundesrat_Impfungen, "bundesrat_Impfungen.xlsx")
write.xlsx(bundesrat_Ski_Wallis, "bundesrat_Ski_Wallis.xlsx")
write.xlsx(bundesrat_Masken, "bundesrat_Masken.xlsx")
write.xlsx(bundesrat_Spital, "bundesrat_Spital.xlsx")

koch_Impfungen <- analysis_verb('koch/propn*', qCorp_Impfungen_lemma)
koch_Ski_Wallis <- analysis_verb('koch/propn*', qCorp_Ski_Wallis_lemma)
koch_Masken <- analysis_verb('koch/propn*', qCorp_Masken_lemma)
koch_Spital <- analysis_verb('koch/propn*', qCorp_Spital_lemma)

write.xlsx(koch_Impfungen, "koch_Impfungen_lemma.xlsx")
write.xlsx(koch_Ski_Wallis, "koch_Ski_Wallis_lemma.xlsx")
write.xlsx(koch_Masken, "koch_Masken_lemma.xlsx")
write.xlsx(koch_Spital, "koch_Spital_lemma.xlsx")

sein_KWIC_Impfungen <- analysis_Sein_KWIC(qCorp_Impfungen)

ggplot(txt_ids_Impfungen, mapping = aes(x = KW)) +
  geom_bar() +
  xlab("Publikationsdatum") +
  ylab('# Texte')

ggplot(txt_ids_Ski_Wallis, mapping = aes(x = KW)) +
  geom_bar() +
  xlab("Publikationsdatum") +
  ylab('# Texte')

ggplot(txt_ids_Masken, mapping = aes(x = KW)) +
  geom_bar() +
  xlab("Publikationsdatum") +
  ylab('# Texte')

ggplot(txt_ids_Spital, mapping = aes(x = KW)) +
  geom_bar() +
  xlab("Publikationsdatum") +
  ylab('# Texte')

txt_ids_All <- bind_rows(txt_ids_Masken,txt_ids_Impfungen) |> bind_rows(txt_ids_Ski_Wallis) |> bind_rows(txt_ids_Spital)

ggplot(txt_ids_All, mapping = aes(x = KW, fill = Topic)) +
  geom_bar() +
  labs(title = 'Anzahl Texte Pro Woche') +
  xlab("Publikationsdatum in Kalenderwoche") +
  ylab('# Texte') +
  facet_wrap(~Topic) +
  theme(legend.position = "none")
