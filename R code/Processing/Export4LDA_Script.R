#### Desired Output Selector ####
#des_pos = c("NOUN", "PROPN", "VERB", "ENTITY")
des_pos = c("NOUN", "PROPN", "ENTITY")

data4LDA = myParsedData |> filter(pos %in% des_pos) # Selektion gewünschter POS

data4LDA = data4LDA |> select(doc_id, lemma) 

txtlist = data4LDA |> select(doc_id) |> unique()

#### Output Preparation ####
outData <- data.frame(doc_id=character(), text=character())
pb <- winProgressBar(min = 0, max = 18712, initial = 0, 
               width = 1000)

#### Text Loop ####
for (i in 1:18712){
  setWinProgressBar(pb, i, title = paste(as.character(i), "/18712"), label = paste(as.character(i), "/18712"))
  temp = data4LDA |> filter(doc_id == txtlist[i,1])
  tempstr = str_flatten(temp$lemma, collapse=" ")
  outData <- outData |>
    add_row(doc_id = as.character(txtlist[i,1]), text = tempstr)
}
close(pb)
write.csv2(outData, 'outData_L_NPE.csv', fileEncoding = "UTF-8")
