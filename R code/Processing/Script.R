#### Imports ####
require(readtext)
require(spacyr)
require(quanteda)
require(openxlsx)
require(tidyverse)
require(gganimate)

#### load data ####

myData = read.xlsx("Data_Raw.xlsx", 1)

myData = myData |> filter(head != 'Nachrichten') |> filter(head != 'NEWS')

myData$headline = str_c(myData$head, ". ", myData$subhead)

myData = myData %>% unite("FullText", c(11:13,15), sep = ".<>", na.rm = T)

myData = myData |> mutate(FullText = str_replace_all(FullText, "<.*?>", " "))

myData$pubtime <- as.Date(myData$pubtime, origin = "1899-12-30")
myData$KW <- as.integer(strftime(myData$pubtime, format = "%V"))

myData = myData |> select(id, pubtime, KW, medium_code, rubric, FullText)

write.xlsx(myData, "Data_Pro4.xlsx")

qCorpus <- corpus(myData, text_field = "FullText", docid_field = "id")


#### Data Overview ####
myPublications <- myData %>% group_by(medium_code) %>% summarize(count=n())

myPubTime <- myData %>% group_by(pubtime, medium_code, KW) %>% summarize(count=n())

myPubTime$weekday <- weekdays(myPubTime$pubtime)

#### Overview Vis ####
anim <- ggplot(myPubTime, mapping = aes(x = pubtime, y = count, fill = medium_code, color = medium_code)) +
  geom_line()+
  xlab("Publikationsdatum")+
  ylab("Anzahl Texte")+
  labs(color='Publikation', title = "Anzahl Texte pro Tag und Publikation")

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

quanCorpus <- corpus(myData,text_field = "FullText")

arrange(summary(quanCorpus), desc(Tokens))

#### Prep Spacy ####
### Parsed Tagged und Entity erkannt
spacy_initialize(model = "de_core_news_sm")

myParsedData <- spacy_parse(myData$FullText)
consolidatedParsedData <- entity_consolidate(myParsedData)

#write.csv2(myParsedData, "parsedData.csv", sep="|")
write.table(myParsedData, "parsedData.txt", sep = "|")
write.table(consolidatedParsedData, "consParsedData.txt", sep = "|")
spacy_finalize()

temp = consolidatedParsedData |> filter(pos == "ENTITY") |> group_by(lemma) |> summarise(count = n()) |> filter(count >= 100) |> arrange(desc(count))
write.csv2(temp, "entities.csv")
spacyToks = as.tokens(consolidatedParsedData, include_pos = "pos")
