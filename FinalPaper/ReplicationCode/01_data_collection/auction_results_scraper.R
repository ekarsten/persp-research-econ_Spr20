#===================================
# Scraping Auction Results from EnergyNet
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

# Rselenium got reomvoed from CRAN :(
# and I unfortunately can't do it all in rvest
#devtools::install_github("ropensci/RSelenium")

# url <- "https://cran.r-project.org/src/contrib/Archive/RSelenium/RSelenium_1.7.5.tar.gz"
# pkgFile <- "RSelenium_1.7.5.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages(pkgs=pkgFile, type="source", repos=NULL)


library(tidyverse)
library(rvest)
library(RSelenium)
library(lubridate)

url_list = c(
  "https://www.energynet.com/govt_listing.pl?sg=3",
  "https://www.energynet.com/govt_listing.pl?sg=4",
  "https://www.energynet.com/govt_listing.pl?sg=265",
  "https://www.energynet.com/govt_listing.pl?sg=1036",
  "https://www.energynet.com/govt_listing.pl?sg=1292",
  "https://www.energynet.com/govt_listing.pl?sg=1809",
  "https://www.energynet.com/govt_listing.pl?sg=1814",
  "https://www.energynet.com/govt_listing.pl?sg=1816",
  "https://www.energynet.com/govt_listing.pl?sg=1822",
  "https://www.energynet.com/govt_listing.pl?sg=2344",
  "https://www.energynet.com/govt_listing.pl?sg=2353",
  "https://www.energynet.com/govt_listing.pl?sg=2878",
  "https://www.energynet.com/govt_listing.pl?sg=2629",
  "https://www.energynet.com/govt_listing.pl?sg=3666"
)

# get selenium going
rD <- rsDriver(browser = "firefox", port = 14L)
remDr <- rD$client

#===================================
# Find Urls to pull tables from
#===================================


get_tract_url <- function(url) {
  # navigate to base page url
  remDr$navigate(url)
  
  # get list of button URLs
  tract_buttons <- remDr$findElements(using = "class name", "btn-lot-info")
  map(tract_buttons, function(x) { x$getElementAttribute("href")}) %>% unlist()
}

mega_url_list <- map(url_list, get_tract_url) %>% unlist()

#===================================
# Navigate to URL, Expand Table, Scrape Both Tables
#===================================

url <- mega_url_list[48]

get_results <- function(url) {
  # go to page and see all bids
  remDr$navigate(paste0(url, "&full_bid_history=1"))
  
  # get html
  page <- remDr$getPageSource()[[1]] %>% read_html()
  
  # extract the descriptive table
  desc_vals <-
    page %>%
    html_node(xpath = "/html/body/div[2]/div/div/div/section[2]/div/div/div[2]/div/div/div/div/div") %>%
    html_text() %>%
    str_remove("Lease No.") %>%
    str_split("County, State|Gross Acres|Net Acres|Royalty Rate|Lease Term|Rental\\/Acre \\(Per Year\\)|Rental Due At Closing|ND Administrative Fee|Buyer Premium") %>%
    unlist()

  name_vector <- function(vec) {
    names(vec) = c("Bidder", "Time", "Bid")
    as.data.frame.list(vec)
  }
  
  # extract the bids table  
  bid_list <-
    page %>%
    html_node(xpath = "/html/body/div[2]/div/div/div/section[2]/div/div/div[4]/div/div/div") %>%
    html_text() %>%
    str_split("\n\\s+\n") %>%
    unlist(recursive = FALSE) %>%
    map(function(x) {str_split(x, "\n")}) %>%
    unlist(recursive = FALSE) %>%
    head(-1) %>%
    tail(-2) %>%
    map(str_trim) %>%
    map_df(name_vector)
  
  final_tab <-
    bid_list %>%
    mutate(
      lease_id = desc_vals[1],
      county = desc_vals[2],
      gross_acres = desc_vals[3],
      net_acres = desc_vals[4],
      royalty_rate = desc_vals[5],
      lease_term = desc_vals[6],
      rental_per_acre = desc_vals[7],
      rental_at_closing = desc_vals[8],
      admin_fee = desc_vals[9],
      buyer_premium = desc_vals[10]
    )
}

#===================================
# Run the scraper!!
#===================================

scraped <-
  map_df(mega_url_list, get_results)

nd_results <- scraped

save(nd_results, file = file.path(ddir, "scraped_data", "nd_results.Rda"))
