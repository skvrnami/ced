rm(list = ls())
library(tesseract)
library(dplyr)

tesseract_download("ces")
eng <- tesseract("ces")

text <- tesseract::ocr("/Users/vtitl/Documents/GitHub/ced/donation_data/primary_data_extracted/vfz2020-ano.pdf", engine = eng)
cat(text)



dataAll = data.frame()

for (i in 1:length(text)){
  page = text[i]
  page <- gsub("\\|", "", page)
  
  rows <- strsplit(page, "\n")[[1]]
  data_list <- lapply(rows, function(row) strsplit(row, ",")[[1]])
  data <- do.call(rbind, data_list)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  dataAll = bind_rows(dataAll, data)
}



write.csv(dataAll, file = "/Users/vtitl/Documents/GitHub/ced/donation_data/primary_data_extracted/vfz2020-ano.csv", row.names = FALSE)