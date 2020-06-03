# Cleaning ND desc and prod data
# Eric Karsten ekarsten@uchicago.edu
# February 2020


# Packages
library(tidyverse)
library(sf)
library(lubridate)


main_crs <- 4269
utm_crs <- 32614

# Repo Setup
root <- getwd()
while(basename(root) != "overlappingauctions") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

# ==============================================================================
# Load Data
# ==============================================================================

load(file.path(ddir, "scraped_data", "nd_auctions.Rda"))
load(file.path(ddir, "scraped_data", "nd_notices.Rda"))
load(file.path(ddir, "scraped_data", "nd_results.Rda"))

# Notices
clean_notices <-
  nd_notices %>%
  mutate(lease_id = str_remove(lease, "\\-")) %>%
  select(lease_id, provisions, nominator) %>%
  mutate(provisions = if_else(is.na(provisions), "", provisions))

unique_provisions <- 
  clean_notices$provisions %>% 
  str_extract_all(boundary("character")) %>% 
  unlist() %>% 
  unique() %>%
  sort()

create_prov_col <- function(df, p) {
  colname = paste0(p, "_prov")
  mutate(df, !!colname := if_else(str_detect(provisions, p), 1, 0))
}

clean_notices <-
  reduce(unique_provisions, create_prov_col, .init = clean_notices) %>%
  select(-provisions)

# Valid ND county names
valid_counties = c("WALSH", "SIOUX", "BURKE", "RAMSEY", "SARGENT", "GRAND FORKS",
                   "MERCER", "DICKEY", "BARNES", "BURLEIGH", "WARD", "BOTTINEAU",
                   "BENSON", "STARK", "BOWMAN", "LOGAN", "EMMONS", "SHERIDAN",
                   "TRAILL", "MCHENRY", "LAMOURE", "KIDDER", "NELSON", "STEELE",
                   "WELLS", "GRIGGS", "SLOPE", "DIVIDE", "MCINTOSH", "ADAMS",
                   "CAVALIER", "WILLIAMS", "CASS", "BILLINGS", "MOUNTRAIL",
                   "MCLEAN", "FOSTER", "PEMBINA", "RICHLAND", "GRANT",
                   "MCKENZIE", "RANSOM", "TOWNER", "RENVILLE", "STUTSMAN",
                   "GOLDEN VALLEY", "OLIVER", "DUNN", "ROLETTE", "MORTON",
                   "PIERCE", "EDDY","HETTINGER")

# Results
auctions <-
  nd_auctions %>%
  as_tibble() %>%
  mutate(year = str_extract(auction, "\\d+"),
         online = str_detect(auction, "Online"),
         month = str_extract(auction, "^\\w+"),
         bonus = as.numeric(`Bonus $/Acre`),
         sec = str_extract(`Twp-Rng-Sec`, "(?<=\\-)\\d+$"),
         twp = str_extract(`Twp-Rng-Sec`, "^\\d+(?=\\-)"),
         rng = str_extract(`Twp-Rng-Sec`, "(?<=^\\d{1,5}\\-)\\d+(?=\\-)")) %>%
  filter(year > 2011, year < 2018) %>%
  mutate(auction_date = mdy(paste(month, "-1-", year))) %>%
  mutate(lease_id = str_remove(`Lease No`, "OG"),
         county = toupper(county)) %>%
  rename(acres = `Mineral Acres`, ) %>%
  select(-`Lease No`, -`Twp-Rng-Sec`, -`Bonus $/Acre`, -auction, -month, -year) %>%
  mutate(lease_id = case_when(
    lease_id == "1300308" ~ "1300307",
    lease_id == "1400002" ~ "1400001",
    lease_id == "1400536" ~ "1400535",
    lease_id == "1400537" ~ "1400535",
    TRUE ~ lease_id
  )) %>%
  group_by(lease_id) %>%
  summarise(
    Bidder = unique(Bidder),
    acres = sum(acres),
    county = unique(county),
    online = unique(online),
    bonus = unique(bonus), 
    sec = unique(sec),
    twp = unique(twp),
    rng = unique(rng),
    auction_date = unique(auction_date)
  ) %>%
  left_join(clean_notices) %>%
  filter(county %in% valid_counties) %>%
  group_by(lease_id) %>%
  filter(n() == 1)

acceptable_auctions <- auctions$lease_id %>% unique()

# deal with paried parcels, they should be grouped and named after the smallest
# 1300307 + 1300308, 1400001 + 1400002, 1400535 + 1400536 + 1400537

# Bids
bids <-
  nd_results %>%
  mutate(Time = if_else(str_detect(Time, "^\\d[:punct:]"), paste0("0", Time), Time),
         bid = as.numeric(str_remove_all(Bid, "[\\$,\\,]")),
         admin_fee = as.numeric(str_remove_all(admin_fee, "[\\$,\\,\\%]")),
         lease_id = str_remove_all(str_remove_all(lease_id, "[OG\\-]"), "\\,\\s{0,4}\\d+"),
         county = toupper(str_extract(county, "\\w+(?=\\,)"))) %>%
  mutate(Time = if_else(str_detect(Time, "\\s\\d{1,2}\\:\\d{2}\\s\\w{2}$"),
                        paste0(str_remove(Time, "\\s\\w{2}$"), ":00", str_extract(Time, "\\s\\w{2}$")),
                        Time)) %>%
  as_tibble() %>%
  mutate(time = as.POSIXct(as_datetime(Time, format="%m/%d/%Y %r"))) %>%
  group_by(lease_id) %>%
  mutate(prior_time = lead(time)) %>%
  mutate(clean_time = if_else(time < prior_time , prior_time + seconds(1), time)) %>%
  mutate(clean_time = if_else(is.na(clean_time), time, clean_time)) %>%
  ungroup() %>%
  select(-Bid, -prior_time, -Time, -gross_acres, -net_acres, -royalty_rate,
         -lease_term, -buyer_premium, -rental_per_acre, -rental_at_closing) %>%
  mutate(autobid = clean_time > time) %>%
  filter(year(time) > 2012, lease_id %in% acceptable_auctions)

# ==============================================================================
# For each auction, compute the number of distinct firms already operating adjacent leases
# ==============================================================================

lease_shapes <-
  file.path(ddir, "raw_data/ND-Lease-Polygons-19000101-20200131") %>%
  st_read() %>%
  mutate(lease_id = str_remove(RecordNo, "OG")) %>%
  st_transform(utm_crs) %>% #convert to a coordinate system where distance is meaningful
  st_buffer(10) # add a buffer of 10m around each shape to capture leases being ajacent


# left data includes shapes, auction date, record number
left_shapes <-
  lease_shapes %>%
  left_join(auctions) %>%
  select(lease_id, auction_date) %>%
  filter(!is.na(auction_date)) %>%
  mutate(row.id = row_number())

# Right data includes shapes, InstDate, ExprDate
right_shapes <-
  lease_shapes %>%
  select(InstDate, ExprDate, AlsGrantee) %>%
  mutate(col.id = row_number())


# use buffer intersection trick to do adjacency
big_intersection <-
  st_intersects(left_shapes, right_shapes) %>%
  data.table::as.data.table() %>%
  as_tibble()


# only keep intersections for which the right lease is active at the auction date of the left one
# group by left auctions and summarise number of neighbors for each record number
adjacency_df <-
  big_intersection %>%
  left_join(st_set_geometry(left_shapes, NULL)) %>%
  left_join(st_set_geometry(right_shapes, NULL)) %>%
  filter(auction_date > InstDate,
         auction_date < ExprDate) %>%
  group_by(lease_id) %>%
  summarise(num_adjacent = length(unique(AlsGrantee)))

auctions <-
  auctions %>%
  left_join(adjacency_df) %>%
  replace_na(list("num_adjacent" = 0))

# ==============================================================================
# Save Clean
# ==============================================================================

save(auctions, file = file.path(ddir, "clean_data", "auctions.Rda"))
save(bids, file = file.path(ddir, "clean_data", "bids.Rda"))
