#===================================
# Turn Auction notices into a clean dataset
# Eric Karsten
# eric.t.karsten@gmail.com
# Project Started February 2020
#===================================

#===================================
# Basic Setup
#===================================

root <- getwd()
while(basename(root) != "overlappingauctions") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

#===================================
# Packages We will need
#===================================

library(tidyverse)
library(lubridate)
library(pdftools)

#===================================
# Packages We will need
#===================================

files_list <- list.files(
  file.path(ddir, "raw_data", "notices"),
  pattern = ".pdf",
  full.names = TRUE)

#===================================
# Function to parse notices
#===================================

parse_notice <- function(path) {
  messy_text <- pdf_text(path)
  
  big_text <- map_df(messy_text,
                     function(x) {
                       str_split(x,"\n") %>%
                         unlist %>%
                         str_remove_all("\r|\n") %>%
                         enframe(name = NULL)})
  
  clean_cols <-
    big_text %>%
    mutate(
      lease = str_extract(value, "(?<=\\s)\\d{2}\\-\\d{5}"),
      provisions = str_extract(value, "^\\s*\\w+(?=\\s+\\d{2})"),
      twp = str_extract(value, "(?<=\\-\\d{5}\\s{1,10})\\d{3}"),
      rng = str_extract(value, "(?<=\\-\\d{5}\\s{1,10}\\d{3}\\s{1,10})\\d{2,3}(?=\\s)"),
      sec = str_extract(value, "(?<=\\-\\d{5}\\s{1,10}\\d{3}\\s{1,10}\\d{2,3}\\s{1,10})\\d{1,2}(?=\\s)"),
      min_acres = str_extract(value, "(?<=\\s)\\d{1,4}\\.\\d{2}"),
      nominator = str_trim(str_extract(value, "(?<=\\.\\d{2}\\s{1,10})[^\\.]+(?=(\\.$)|$)|(?<=\\.\\d{2}\\s{1,10})[^\\d]+(?=(\\.$)|$)"))
    ) %>%
    filter(!is.na(lease)) %>%
    select(-value)
  
  return(clean_cols)
}

nd_notices <- map_df(files_list, parse_notice)

# Note for parsing provisions
# P indicates there may be palenotological resources needing protection
# R indicates no surface occupancy allowed
# S indicates wildlife habitat managed by Comissioner
# W indicates wildlife habitat not managed by department of land trusts
# H is historical resource


save(nd_notices, file = file.path(ddir, "scraped_data", "nd_notices.Rda"))



