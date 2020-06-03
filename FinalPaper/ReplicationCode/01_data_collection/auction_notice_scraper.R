#===================================
# Scraping Auction data from ND's site
# Eric Karsten
# eric.t.karsten@gmail.com
# Project Started January 2019
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
library(rvest)
library(RSelenium)
library(lubridate)

# times we want to capture
yrs <- 2010:2018 %>% as.character()
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

dates <-
  expand.grid(year = yrs, month = months) %>%
  mutate(excel = paste0("https://land.nd.gov/Docs/Minerals/",
                        year, month, "worksheet.xlsx"),
         pdf = paste0("https://land.nd.gov/Docs/Minerals/",
                      year, month, "auctionlist.pdf"))

url <- "https://land.nd.gov/Minerals/AuctionHistorySale"

# get selenium going
rD <- rsDriver()
remDr <- rD$client
remDr$navigate(url)

#===================================
# Extracting the pages to iterate over
#===================================

site <- read_html(url)

page_values <-
  site %>%
  html_nodes("option") %>%
  html_attr("value") %>%
  unlist()

page_text <-
  site %>%
  html_nodes("option") %>%
  html_text()

all_pages <-
  tibble(value = page_values,
         auction = page_text)

dates <-
  all_pages %>%
  mutate(xls = paste0("https://land.nd.gov/Docs/Minerals/",
                      str_extract(value, "^\\d{6}"),
                      "worksheet.xlsx"),
         pdf = paste0("https://land.nd.gov/Docs/Minerals/",
                      str_extract(value, "^\\d{6}"),
                      "auctionlist.pdf"),
         filename = file.path(ddir, "raw_data", "notices",
                              str_extract(value, "^\\d{6}")))

map2(dates$pdf, paste0(dates$filename, ".pdf"),
     function(x,y) {try(download.file(x,y, mode = "wb"), silent = T)})

map2(dates$xls, paste0(dates$filename, ".xlsx"),
     function(x,y) {try(download.file(x,y, mode = "wb"), silent = T)})

