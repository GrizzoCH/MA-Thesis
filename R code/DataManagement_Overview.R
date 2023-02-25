#### Imports ####
require(readtext)
require(spacyr)
require(quanteda)
require(quanteda.textplots)
require(quanteda.textstats)
require(openxlsx)
require(tidyverse)
require(gganimate)

#### load data ####
myData = read.xlsx("Data_Raw.xlsx", 1)

#### Data cleaning ####
myData = myData |> filter(head != 'Nachrichten') |> filter(head != 'NEWS')

myData$headline = str_c(myData$head, ". ", myData$subhead)

myData = myData %>% unite("FullText", c(11:13,15), sep = ".<>", na.rm = T)

myData = myData |> mutate(FullText = str_replace_all(FullText, "<.*?>", " "))

myData$pubtime <- as.Date(myData$pubtime, origin = "1899-12-30")
myData$KW <- as.integer(strftime(myData$pubtime, format = "%V"))

myData = myData |> select(id, pubtime, KW, medium_code, rubric, FullText)

write.xlsx(myData, "Data_Pro4.xlsx")

#### Prep Spacy ####
### Parsed Tagged und Entity erkannt
spacy_initialize(model = "de_core_news_sm")

parseTable <- myData |> select(doc_id, FullText) 
parseTable <- parseTable |> rename(text = FullText)

myParsedData <- spacy_parse(parseTable)
consolidatedParsedData <- entity_consolidate(myParsedData)
entities <- entity_extract(myParsedData)

temp = entities |> group_by(entity) |> summarise(count = n()) |> arrange(desc(count))
write.csv2(temp, "entities.csv") # Export Frequenzen Entities

write.table(myParsedData, "parsedData.txt", sep = "|") # Export POS Tagging & Lemmatisierung
saveRDS(myParsedData, 'parsedData.rda') # Export in RDS Format
write.table(consolidatedParsedData, "consParsedData.txt", sep = "|")
spacy_finalize()




#### Data Overview ####
myPublications <- myData %>% group_by(medium_code) %>% summarize(count=n())

myPubTime <- myData %>% group_by(pubtime, medium_code, KW) %>% summarize(count=n())

myPubTime$weekday <- weekdays(myPubTime$pubtime)

#### Overview Visuals ####
ggplot(myPubTime, mapping = aes(x = pubtime, y = count, fill = medium_code)) +
  geom_area()+
  xlab("Publikationsdatum")+
  ylab("Anzahl Texte")+
  labs(fill = 'Publikation', title = "Anzahl Texte pro Tag und Publikation")

anim <- ggplot(myPublications, mapping = aes(x = medium_code, y = count, fill = medium_code)) +
  geom_col()+
  xlab("Publikation")+
  ylab("Anzahl Texte")+
  labs(fill='Publikation', title = "Anzahl Texte pro Publikation")+
  geom_label(aes(label = count))

temp <- myPubTime |> group_by(KW, medium_code) |> summarise(count = sum(count))
  
anim <- ggplot(temp, mapping = aes(x = KW, y = count, color = medium_code, group = medium_code)) +
  geom_line() +
  geom_point() +
  transition_reveal(KW)

animate(anim, renderer = av_renderer())

anim_save("PublicationsoverTime.gif")

#spacyToks = as.tokens(myParsedData, include_pos = "pos")

#### Token pro Text Pro Publikation
qCorpus <- corpus(myData, text_field = "FullText", docid_field = "id")

txt_summ <- textstat_summary(qCorpus)

sum(txt_summ$tokens)

test <- tokens(myData$FullText)

temp <- myData |> select(id, medium_code)

txt_summ$id <- as.numeric(txt_summ$document)

txt_summ <- txt_summ |> left_join(temp)

txt_summ |> group_by(medium_code) |> summarize(Mean = mean(tokens))

txt_summ |> summarize(Mean = mean(tokens))

ggplot(txt_summ, mapping = aes(x = medium_code, y = tokens, fill = medium_code)) +
  geom_boxplot()+
  ylab("Anzahl Tokens")+
  xlab("Publikation")+
  labs(fill = 'Publikation', title = "Anzahl Wörter pro Text und Publikation")
