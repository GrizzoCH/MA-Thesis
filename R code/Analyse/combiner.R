# Analyse Script CombineR
#### Imports ####
require(readtext)

require(quanteda)
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