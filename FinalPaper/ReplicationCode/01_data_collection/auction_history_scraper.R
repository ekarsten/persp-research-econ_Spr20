#===================================
# Scraping Auction data from ND's site
# Eric Karsten
# eric.t.karsten@gmail.com
# Project Started July 2018
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

library(tidyverse)
library(rvest)
library(RSelenium)
library(lubridate)

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

#===================================
# Writing the function that pulls the table from HTML
#===================================

date_value <- all_pages$value[2]

get_leases <- function(date_value) {
  #click the given date value
  date_elem <-
    remDr$findElement(using = 'xpath',
                      paste0("//option[@value='", date_value,"']" ))
  date_elem$clickElement()
  
  # find the submit button
  elem <- remDr$findElement(using = 'xpath', "//input[@type='submit']")
  # click the submit button
  elem$clickElement()
  
  #wait for page to load
  Sys.sleep(2)
  
  # read the html from the page
  page <- remDr$getPageSource()[[1]] %>% read_html()
  
  # use rvest to make the table nice
  bad_tab <-
    page %>%
    html_node("table") %>%
    html_table(fill = T)
  
  if (length(bad_tab$`Lease No`) != 0) {
  
  # this nice table has some nonsense going on
  # we want to get the county rows out of the picture
  
  county_inds <-
    which(is.na(bad_tab$`Twp-Rng-Sec`))
  
  counties <-
    tibble(county = bad_tab[county_inds,]$`Lease No`,
           start = county_inds,
           end = lead(county_inds - 1))
  
  counties$end[length(county_inds)] <- length(bad_tab$`Lease No`)
  
  counties <-
    counties %>%
    mutate(cnt = end - start + 1)
  
  county <-
    map2(counties$county, counties$cnt,
       function(cty, num) {rep(cty, num)}) %>%
    unlist()
  
  out <-
    bad_tab %>%
    mutate(county,
           value = date_value) %>%
    filter(!is.na(`Twp-Rng-Sec`))
  
  return(out)}
}

#===================================
# map_df process to put together all the tables
#===================================

scraped <-
  map_df(all_pages$value, get_leases)

nd_auctions <-
  scraped %>%
  left_join(all_pages) %>%
  select(-value)

nd_auctions <-
  nd_auctions %>%
  mutate(year = str_extract(auction, "\\d+"),
         online = str_detect(auction, "Online"),
         month = str_extract(auction, "^\\w+"),
         bonus = as.numeric(`Bonus $/Acre`)) %>%
  mutate(date = mdy(paste(month, "-1-", year)))

nd_auctions %>%
  filter(year > 2010) %>%
  ggplot(aes(x = date, y = bonus, color = online)) +
  geom_smooth() +
  geom_point()

save(nd_auctions, file = file.path(ddir, "scraped_data", "nd_auctions.Rda"))
