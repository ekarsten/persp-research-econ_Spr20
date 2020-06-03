# Preparing regression data
# Eric Karsten ekarsten@uchicago.edu
# February 2020

# Packages
library(tidyverse)
library(lubridate)
library(lfe)
library(stargazer)
library(xtable)
library(sf)

# Repo Setup
root <- getwd()
while(basename(root) != "overlappingauctions") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))
source(file.path(root, "didata.R"))

main_crs <- 4269
utm_crs <- 32614
di_crs <- 4267


# ==============================================================================
# Load Data
# ==============================================================================

# Data
load(file.path(ddir, "clean_data", "auctions.Rda"))
load(file.path(ddir, "clean_data", "bids.Rda"))
raw_prices <-
  readxl::read_xlsx(file.path(ddir, "raw_data", "prices", "clearbrook.xlsx"),
                    sheet = 2) %>%
  select(Date = Dates, price = PX_LAST) %>%
  mutate(Date = as_date(Date))
nd_desc <- didata("ND", "pden_desc")

nd_desc <- nd_desc %>%
  as_tibble() %>%
  select(spud_date, first_prod_date, latitude, longitude, twp, rng, county,
         peak_gas, peak_liq, lease_id) %>%
  filter(!is.na(first_prod_date)) %>%
  mutate(twp = str_remove(twp, "N"),
         rng = str_remove(rng, "W"),
         county = toupper(str_remove(county, "\\s+\\(ND\\)"))) %>%
  mutate(twp = as.numeric(twp), rng = as.numeric(rng))


lease_shapes <-
  file.path(ddir, "raw_data/ND-Lease-Polygons-19000101-20200131") %>%
  st_read() %>%
  mutate(lease_id = str_remove(RecordNo, "OG")) %>%
  st_transform(utm_crs)

wells <-
  file.path(ddir, "raw_data/Wells") %>%
  st_read()

units <-
  file.path(ddir, "raw_data/DrillingSpacingUnits") %>%
  st_read()

# ==============================================================================
# Compute Quantity for each grid
# ==============================================================================

auction_dates <- auctions$auction_date %>% unique()

all_zones <- expand_grid(twp = 129:164, rng = 67:106)

get_tr_prod <- function(d) {
  lookback_end = as.Date(d) %m-% months(1)
  lookback_sart = as.Date(d) %m-% months(3*12)
  df <- nd_desc %>%
    filter(first_prod_date < lookback_end, first_prod_date > lookback_sart) %>%
    group_by(rng, twp) %>%
    summarise(
      mean_peak_gas = mean(peak_gas),
      mean_peak_liq = mean(peak_liq),
      wells_drilled_3yrs = n()) %>%
    mutate(auction_date = d) %>%
    ungroup()
  
  get_closest <- function(t,r) {
    df %>%
      mutate(dist = (twp - t)^2 + (rng - r)^2,
             min_dist = min(dist),
             close_points = dist < min(dist) + (min(dist) + 12)/3) %>%
      filter(close_points) %>%
      summarise(
        twp = t, rng = r, auction_date = d,
        mean_peak_gas = sum(mean_peak_gas*wells_drilled_3yrs)/sum(wells_drilled_3yrs),
        mean_peak_liq = sum(mean_peak_liq*wells_drilled_3yrs)/sum(wells_drilled_3yrs),
        wells_drilled_3yrs = first(wells_drilled_3yrs),
        wells_drilled_3yrs = if_else(min(dist) == 0, wells_drilled_3yrs, as.integer(0)),
        not_imputed = if_else(min(dist) == 0, 1, 0)
        )
  }
  
  map2_df(all_zones$twp, all_zones$rng, get_closest)
}

auction_prod <- map_df(auction_dates, get_tr_prod)

auctions <- auctions %>%
  mutate(twp = as.numeric(twp), rng = as.numeric(rng)) %>%
  left_join(auction_prod)

# ==============================================================================
# For each parcel, get the past price
# ==============================================================================

get_tr_price <- function(d) {
  lookback_end = as.Date(d)
  lookback_sart = as.Date(d) %m-% months(3)
  out <- raw_prices %>%
    filter(Date < lookback_end, Date > lookback_sart) %>%
    group_by(months_back = paste0("p_back_", as.numeric(floor((d - Date)/31)))) %>%
    summarise(price = mean(price)) %>%
    pivot_wider(names_from = months_back, values_from=price) %>%
    mutate(auction_date = d)
  if(length(out$auction_date == 1)) {
    return(out)
  }
}

auction_prices <- map_df(auction_dates, get_tr_price)

auctions <- auctions %>%
  left_join(auction_prices)

# ==============================================================================
# Flag Royalty Rate
# ==============================================================================

# NOTE FROM THE NOTICES THAT ROYALTY RATE VARIES BY COUNTY: 
# 3/16 in 
# 1/6 in all other counties

spec_counties = toupper(c("Billings", "Divide", "Dunn", "Golden Valley", "McKenzie", "Mountrail", "Williams"))

auctions <- auctions %>%
  mutate(royalty_high = if_else(county %in% spec_counties, 1, 0))

# ==============================================================================
# Get Highest Bid for each lease
# ==============================================================================

auction_bids <-
  bids %>%
  group_by(lease_id) %>%
  summarise(max_bid = max(bid))

regression_data <- auctions %>% left_join(auction_bids)

# ==============================================================================
# Get time to drill and production outcomes for each lease
# ==============================================================================

# Turns out this is impossible because assigning production to leases is a total
# nightmare 

# desc_sf <-
#   nd_desc %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = di_crs) %>%
#   st_transform(utm_crs) %>%
#   select(spud_date, peak_gas, peak_liq)
#   
# 
# lease_prod <-
#   lease_shapes %>%
#   st_join(desc_sf) %>%
#   st_set_geometry(NULL) %>%
#   filter(spud_date > InstDate, spud_date < ExprDate) %>%
#   group_by(lease_id) %>%
#   filter(spud_date == min(spud_date)) %>%
#   ungroup() %>%
#   mutate(drill_time = spud_date - InstDate,
#          undrilled = is.na(spud_date)) %>%
#   select(lease_id, drill_time, undrilled, peak_liq, peak_gas)
# 
# regression_data <- regression_data %>% left_join(lease_prod)
  

save(regression_data, file = file.path(ddir, "clean_data", "regression_data.Rda"))






