
# Joe's Issues ------------------------------------------------------------


# Packages ----------------------------------------------------------------

library(tidyverse)
library(ngramr)
library(ggplot2)
library(RColorBrewer)


# Data 1 ------------------------------------------------------------------


mydata1 <- read_table("totalcounts.txt",
                      col_names = FALSE,
                      col_types = paste0(rep("c", 426),
                                         collapse = "")) %>% 
  pivot_longer(., cols = everything()) %>% 
  separate(., value, c("year", "match_total", "page_total", "volume_total")) %>% 
  select(., -name) %>% 
  mutate_if(., is.character, as.numeric) %>%
  na.omit(.) %>%
  filter(., year >= 1800 & year <= 2000) 


# Data 2  -----------------------------------------------------------------

mydata2 <- read_table("1gram",
                      col_names = c("ngram", "year", "total", "volume")) %>%
  filter(., year >= 1800 & year <= 2000) 


data <- as.data.frame(matrix(ncol = 1, nrow = 209))
data$V1 <- seq(from = 1800, to = 2008)
names(data)[names(data) == "V1"] <- "Year"
search_terms <- c("1883", "1910", "1950")


for(i in 1:length(search_terms)){
  term <- search_terms[i]
  temp <- ngram(term, corpus = "en-2012", year_start = 1800,
                smoothing = 0, count = TRUE, case_ins = FALSE)
  data <- merge(data, temp[,c("Year", "Count")], 
                by = "Year", all.x = TRUE)
  colname <- paste(term, sep = "")
  names(data)[names(data) == "Count"] <- colname
  rm(temp)}

names(data)[names(data) == "1883"] <- "total-term1883"
names(data)[names(data) == "1910"] <- "total-term1910"
names(data)[names(data) == "1950"] <- "total-term1950"

#Total count data
counts_file <- file.path("Downloads", "googlebooks-eng-all-totalcounts-20120701.txt")

if (!dir.exists("data")) {
  dir.create("data")
}


# Issue is Here  ----------------------------------------------------------

if (!file.exists(counts_file)) { 
  download.file(
    "http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-totalcounts-20120701.txt",
    counts_file
  )
}

# -------------------------------------------------------------------------

# total_counts_temp <- t(
#   read.table(
#     counts_file, 
#     header = FALSE
#   )
# )
# 
# total_counts_char <- do.call(
#   rbind, 
#   strsplit(total_counts_temp, ",")
# )

# total_counts <- apply(total_counts_char, 2, as.numeric)
# colnames(total_counts) <- c("Year", "match_count", "page_count", "volume_count")
# 
# data2 <- as.tibble(total_counts)
# 
# data


