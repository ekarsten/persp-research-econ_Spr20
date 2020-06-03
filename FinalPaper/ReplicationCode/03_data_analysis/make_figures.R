# Document where the code for all the figures lives
# Eric Karsten March 2020
# eric.t.karsten@gmail.com

root <- getwd()
while(basename(root) != "overlappingauctions") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

library(tidyverse)
library(lubridate)
library(lfe)
library(stargazer)
library(xtable)
library(scales)
library(splines)
library(ggspatial)

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

caption_at_bottom <- function(expr) {
  x <- capture.output(expr)
  cap <- grep("\\\\caption", x)
  lab <- grep("\\\\label", x)
  last <- grep("\\\\end\\{table", x)
  cat(
    paste(
      c(x[-last], x[cap], x[lab], x[last])[-c(cap, lab)]
      , collapse = "\n")
    , "\n")
}

file_stargazer <- function(output.file, ...) {
  output <- capture.output(caption_at_bottom(stargazer(...)))
  cat(paste(output, collapse = "\n"), "\n", file=output.file)
}


# Data
load(file.path(ddir, "clean_data", "auctions.Rda"))
load(file.path(ddir, "clean_data", "bids.Rda"))
load(file.path(ddir, "clean_data", "regression_data.Rda"))
raw_prices <-
  readxl::read_xlsx(file.path(ddir, "raw_data", "prices", "clearbrook.xlsx"),
                    sheet = 2) %>%
  select(Date = Dates, price = PX_LAST) %>%
  mutate(Date = as_date(Date))
load(file.path(ddir, "clean_data", "entry_results.Rda"))
model_results <- read_csv(file.path(ddir, "clean_data", "solved_lik.csv"))
plotting_df <- read_csv(file.path(ddir, "clean_data", "plotting_dat.csv"))
load(file.path(ddir, "clean_data", "counterfactual_df.Rda"))
simulation_dat <- read_csv(file.path(ddir, "clean_data", "samples_dat.csv"))
lik_data <- read_csv(file.path(ddir, "clean_data", "lik_data_2.csv"))

lease_shapes <-
  file.path(ddir, "raw_data/ND-Lease-Polygons-19000101-20200131") %>%
  st_read() %>%
  mutate(lease_id = str_remove(RecordNo, "OG"))

# ==============================================================================
# Reduced form regs figure
# ==============================================================================
rd <-
  regression_data %>%
  select(-max_bid) %>%
  mutate(twprngyr = paste0(twp, rng, year(auction_date)),
         hit_min = bonus < 3,
         win_nom = nominator == Bidder)


m1 = felm(log(bonus) ~ online | twprngyr, data = rd)
m2 = felm(log(bonus) ~ log(p_back_0) + log(p_back_1) + log(p_back_2) + log(mean_peak_gas + 1) + log(mean_peak_liq + 1) + online, data = rd)
m3 = felm(log(bonus) ~ log(p_back_0) + log(p_back_1) + log(p_back_2) + log(mean_peak_gas + 1) + log(mean_peak_liq + 1) + online + P_prov + R_prov + W_prov + royalty_high, data = rd)
m4 = felm(log(bonus) ~  online + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1), data = rd)
m5 = felm(log(bonus) ~  online + P_prov + R_prov + W_prov + royalty_high + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1), data = rd)


file_stargazer(output.file = file.path(fdir, "fe_regs.tex"),
          m1, m2, m3, m4, m5, omit.stat = c("ser", "F"),
          add.lines = list(c("Year * Location FE", "Yes", "", "", "", ""),
                           c("Price (3 periods) + Quantity", "", "Yes", "Yes", "", ""),
                           c("Spline Price + Quantity", "", "", "", "Yes", "Yes"),
                           c("Lease Provision", "", "", "Yes", "", "Yes")),
          covariate.labels = c("Online"),
          suppress.errors = TRUE,
          dep.var.labels = c("Log(Winning Bid) in \\$/acre"),
          font.size = "small",
          title = "Regressing winning bid on online auction controlling for year and location as proxies for price and mineral quality",
          header = FALSE,
          keep = c("online"),
          table.placement = "h!",
          label = "reduced_regs")

# ==============================================================================
# Prices and auctions figure
# ==============================================================================

# vertical line dates
date_list <- auctions %>%
  ungroup() %>%
  select(auction_date, online) %>%
  unique() %>%
  mutate(online = if_else(online, "Online", "Open Outcry"))

price_timeseries_fig <-
  raw_prices %>%
  ggplot(aes(x = Date, y = price)) +
  geom_line() +
  geom_vline(data = date_list, aes(xintercept = auction_date, color = online)) +
  theme_bw() +
  coord_cartesian(xlim = as.Date(c("2012-01-01", "2017-12-31"))) +
  labs(y = "Price ($/bbl)",
       color = " ")

ggsave(file.path(fdir, "p-timeseries.png"), price_timeseries_fig, width = 6, height = 3, units = "in")

# ==============================================================================
# Summary Statistics Table
# ==============================================================================

bids %>%
  group_by(lease_id) %>%
  summarise(n_p = length(unique(Bidder))) %>%
  right_join(rd) %>%
  select(`Revenue ($/Acre)` = bonus, `Number of Bidders` = n_p, `Tract Size (Acres)` = acres, `Adjacent Leases` = num_adjacent, online) %>%
  mutate(`Log Revenue` = log(`Revenue ($/Acre)`)) %>%
  pivot_longer(-online, names_to = "Variable") %>%
  group_by(online, Variable) %>%
  summarise(value = paste0(round(mean(value, na.rm = T), 2), " (", round(sd(value, na.rm = T), 2), ")")) %>%
  ungroup() %>%
  mutate(value = if_else(str_detect(value, "NA"), "", value),
         online = if_else(online, "Online", "Open Outcry")) %>%
  pivot_wider(names_from = "online") %>%
  arrange(match(Variable, c("Revenue ($/Acre)", "Log Revenue", "Number of Bidders", "Tract Size (Acres)", "Adjacent Leases"), nomatch = 0)) %>%
  xtable(caption = "Summary Statistics: Mean (SD) of several variables in the dataset.", label = "summary_stats") %>%
  print(include.rownames = F, file = file.path(fdir, "summary_stats.tex"))
  

# ==============================================================================
# Example Twp * Rng
# ==============================================================================


shp_df <- lease_shapes %>%
  left_join(select(rd, online, lease_id, twp, rng)) %>%
  filter(!is.na(online))

map_plot <- shp_df %>%
  st_transform(32614) %>%
  filter(twp %in% 138:139, rng %in% 102:104) %>%
  mutate(online = if_else(online, "Online", "Open Outcry")) %>%
  ggplot(aes(fill = online)) +
  geom_sf(alpha = .5) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.4)) +
  labs(x = "Longitude", y = "Latitude", fill = " ")

ggsave(file.path(fdir, "sample_leases.png"), map_plot, height = 4.5, units = "in")

# ==============================================================================
# Test of common values table
# ==============================================================================

# do a raw version, then a version with model 1 residuals and one with model 5 residuals

n1 = lm(log(bonus) ~ online + twprngyr, data = rd)
n5 = lm(log(bonus) ~  online + P_prov + R_prov + W_prov + royalty_high + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1), data = rd)


auction_fitted <-
  rd %>%
  ungroup() %>%
  mutate(fitted_1 = predict(n1, .), fitted_2 = predict(n5, .)) %>%
  select(lease_id, fitted_1, fitted_2, auction_date)

cv_uncontrolled <-
  bids %>%
  left_join(auction_fitted) %>%
  group_by(auction_date) %>%
  mutate(n_a = length(unique(Bidder))) %>%
  group_by(lease_id, auction_date, Bidder) %>%
  summarise(n_a = max(n_a),
            max_bid = max(bid)) %>%
  group_by(lease_id, auction_date) %>%
  summarise(n_p = length(unique(Bidder)),
            b_nm1 = sort(max_bid, TRUE)[2],
            b_nm2 = sort(max_bid, TRUE)[3])

cv_mod1 <-
  bids %>%
  left_join(auction_fitted) %>%
  drop_na() %>%
  group_by(auction_date) %>%
  mutate(n_a = length(unique(Bidder))) %>%
  group_by(lease_id, auction_date, Bidder) %>%
  summarise(n_a = max(n_a),
            max_bid = max(log(bid) - fitted_1)) %>%
  group_by(lease_id, auction_date) %>%
  summarise(n_p = length(unique(Bidder)),
            b_nm1 = sort(max_bid, TRUE)[2],
            b_nm2 = sort(max_bid, TRUE)[3])


cv_mod5 <-
  bids %>%
  left_join(auction_fitted) %>%
  drop_na() %>%
  group_by(auction_date) %>%
  mutate(n_a = length(unique(Bidder))) %>%
  group_by(lease_id, auction_date, Bidder) %>%
  summarise(n_a = max(n_a),
            max_bid = max(log(bid) - fitted_2)) %>%
  group_by(lease_id, auction_date) %>%
  summarise(n_p = length(unique(Bidder)),
            b_nm1 = sort(max_bid, TRUE)[2],
            b_nm2 = sort(max_bid, TRUE)[3])

make_cv_test <- function(df, f, cap, lab) {
  df %>%
    mutate(rhs = 2/n_p * b_nm2 + (n_p-2)/n_p * b_nm1 ) %>%
    group_by(n_p) %>%
    summarise(
      sd_lhs = sd(b_nm1),
      sd_rhs = sd(rhs),
      b_nm1 = mean(b_nm1),
      b_nm2 = mean(b_nm2),
      rhs = mean(rhs),
      obs = n(),) %>%
    mutate(lhs = lag(b_nm1),
           lhs_obs = lag(obs),
           sd_lhs = lag(sd_lhs),
           rhs_obs = obs) %>%
    filter(lag(n_p) == n_p -1, !is.na(b_nm2), !is.na(sd_lhs), !is.na(sd_rhs)) %>%
    select(n_p, lhs, rhs, sd_lhs, sd_rhs, lhs_obs, rhs_obs) %>%
    mutate(diff = lhs - rhs, pooled_sd = sqrt( ((lhs_obs - 1)*sd_lhs^2+ (rhs_obs -2)*sd_rhs^2)/(lhs_obs + rhs_obs - 2))) %>%
    mutate(t = diff/(pooled_sd*(1/rhs_obs + 1/lhs_obs))) %>%
    mutate(p = pt(t, lhs_obs + rhs_obs - 2)) %>%
    round(2) %>%
    mutate(n = as.integer(n_p),
           LHS = paste0(lhs, " (", sd_lhs, ")"),
           RHS = paste0(rhs, " (", sd_rhs, ")"),
           Difference = diff,
           lhs_obs = as.integer(lhs_obs),
           rhs_obs = as.integer(rhs_obs)) %>%
    select(n, LHS, RHS, Difference, p, `$LHS_{obs}$` = lhs_obs, `$RHS_{obs}$` = rhs_obs) %>%
    xtable(caption = cap, label = lab) %>%
    print(include.rownames = F, file = file.path(fdir, f),
          sanitize.text.function = function(str) {str})
}

make_cv_test(cv_uncontrolled, "cv_test_uncontrolled.tex",
             "Athey and Levin test for common values in Acending Auctions, No Controls for parcel value",
             "cv_uncontrolled")

make_cv_test(cv_mod1, "cv_test_mod1.tex",
             "Athey and Levin test for common values in Acending Auctions, Reduced form model 1 Controls for parcel value",
             "cv_mod1")

make_cv_test(cv_mod5, "cv_test_mod5.tex",
             "Athey and Levin test for common values in Acending Auctions, Reduced form model 5 Controls for parcel value",
             "cv_mod5")


# Figure showing CDFs of winning values corresponding to each number of bidders for each auction format

mod1_cdf <- cv_mod1 %>%
  filter(n_p < 7) %>%
  mutate(n_p = as.factor(n_p)) %>%
  ggplot(aes(x = b_nm1, color = n_p)) +
  stat_ecdf() +
  theme_bw() +
  labs(x = "Second Highest Bid/Value",
       color = "Number of Bidders",
       y = "Cumulative Density") +
  theme(legend.position = "bottom")

mod5_cdf <- cv_mod5 %>%
  filter(n_p < 7) %>%
  mutate(n_p = as.factor(n_p)) %>%
  ggplot(aes(x = b_nm1, color = n_p)) +
  stat_ecdf() +
  theme_bw() +
  labs(x = "Second Highest Bid/Value",
       color = "Number of Bidders",
       y = "Cumulative Density") +
  theme(legend.position = "bottom")

ggsave(file.path(fdir, "mod1_cdf.png"), mod1_cdf, width = 6, height = 4, units = "in")
ggsave(file.path(fdir, "mod5_cdf.png"), mod5_cdf, width = 6, height = 4, units = "in")


# ==============================================================================
# Minearal over space and time
# ==============================================================================

# figures of variation in expected production over space and time
gas_q <- regression_data %>%
  ggplot(aes(x = -as.numeric(rng), y = as.numeric(twp), fill = log(mean_peak_gas + 1))) +
  geom_tile() +
  facet_wrap(~year(auction_date)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Range", y = "Township", fill = "Log(Mean Quantity Gas)")

liq_q <- regression_data %>%
  ggplot(aes(x = -as.numeric(rng), y = as.numeric(twp), fill = log(mean_peak_liq + 1))) +
  geom_tile() +
  facet_wrap(~year(auction_date)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Range", y = "Township", fill = "Log(Mean Quantity Liquid)")

ggsave(file.path(fdir, "gas_q.png"), gas_q, width = 6, height = 4, units = "in")
ggsave(file.path(fdir, "liq_q.png"), liq_q, width = 6, height = 4, units = "in")


# ==============================================================================
# Minearal between auctions
# ==============================================================================

# figures of variation in expected production over space and time
auction_loc <- regression_data %>%
  ggplot(aes(x = -as.numeric(rng), y = as.numeric(twp), fill = online)) +
  geom_tile() +
  facet_grid(year(auction_date)~online) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Range", y = "Township", fill = "Online")

ggsave(file.path(fdir, "auction_log.png"), auction_loc, width = 6, height = 8, units = "in")


regression_data %>%
  group_by(rng, twp) %>%
  summarise(all_online = all(online),
            all_offline = all(!online)) %>%
  ungroup() %>%
  summarise(prop_all_online = mean(all_online),
            prop_all_offline = mean(all_offline))

# ==============================================================================
# Achieving minimum bid
# ==============================================================================

c1 = lm(hit_min ~ online + twprngyr, data = rd)
c2 = lm(hit_min ~ log(p_back_0) + log(p_back_1) + log(p_back_2) + log(mean_peak_gas + 1) + log(mean_peak_liq + 1) + online, data = rd)
c3 = lm(hit_min ~ log(p_back_0) + log(p_back_1) + log(p_back_2) + log(mean_peak_gas + 1) + log(mean_peak_liq + 1) + online + P_prov + R_prov + W_prov + royalty_high, data = rd)
c4 = lm(hit_min ~  online + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1), data = rd)
c5 = lm(hit_min ~  online + P_prov + R_prov + W_prov + royalty_high + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1), data = rd)

file_stargazer(output.file = file.path(fdir, "hit_min_regs.tex"),
               c1, c2, c3, c4, c5, omit.stat = c("ser", "F"),
               add.lines = list(c("Year * Location FE", "Yes", "", "", "", ""),
                                c("Price (3 periods) + Quantity", "", "Yes", "Yes", "", ""),
                                c("Spline Price + Quantity", "", "", "", "Yes", "Yes"),
                                c("Lease Provision", "", "", "Yes", "", "Yes")),
               covariate.labels = c("Online"),
               suppress.errors = TRUE,
               dep.var.labels = c("Win at Minimum Bid"),
               font.size = "footnotesize",
               title = "Linear probability regressions on indicator for winning parcel at minimum bid.",
               header = FALSE,
               keep = c("online"),
               table.placement = "h!",
               label = "hit_min")

# ==============================================================================
# Winning own nomination
# ==============================================================================

d1 = lm(win_nom ~ online + twprngyr, data = rd)
d2 = lm(win_nom ~ log(p_back_0) + log(p_back_1) + log(p_back_2) + log(mean_peak_gas + 1) + log(mean_peak_liq + 1) + online, data = rd)
d3 = lm(win_nom ~ log(p_back_0) + log(p_back_1) + log(p_back_2) + log(mean_peak_gas + 1) + log(mean_peak_liq + 1) + online + P_prov + R_prov + W_prov + royalty_high, data = rd)
d4 = lm(win_nom ~  online + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1), data = rd)
d5 = lm(win_nom ~  online + P_prov + R_prov + W_prov + royalty_high + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1), data = rd)

file_stargazer(output.file = file.path(fdir, "win_nom_regs.tex"),
               d1, d2, d3, d4, d5, omit.stat = c("ser", "F"),
               add.lines = list(c("Year * Location FE", "Yes", "", "", "", ""),
                                c("Price (3 periods) + Quantity", "", "Yes", "Yes", "", ""),
                                c("Spline Price + Quantity", "", "", "", "Yes", "Yes"),
                                c("Lease Provision", "", "", "Yes", "", "Yes")),
               covariate.labels = c("Online"),
               suppress.errors = TRUE,
               dep.var.labels = c("Winner is the Nominator"),
               font.size = "footnotesize",
               title = "Linear probability regressions on indicator for the same bidder nominating and winning a parcel",
               keep = c("online"),
               table.placement = "h!",
               label = "win_nom")

# ==============================================================================
# Late bidding behavior
# ==============================================================================

ld <- auctions %>%
  select(lease_id, auction_date) %>%
  right_join(bids) %>%
  group_by(auction_date) %>%
  mutate(end_time = max(clean_time)) %>%
  ungroup() %>%
  mutate(hours_from_end = as.numeric(clean_time - end_time)/3600)

max_bid_dist <- ld %>%
  group_by(Bidder, lease_id) %>%
  summarise(last_bid = max(hours_from_end, na.rm = TRUE)) %>%
  ggplot(aes(x = -(last_bid - 1))) +
  stat_ecdf() +
  scale_x_continuous(trans = reverselog_trans(10),
                     labels = trans_format("identity", function(x) -x + 1),
                     breaks = c(1:7, 9, 1:5 * 10 + 1, 76,  101, 151, 201)) +
  theme_bw() +
  labs(x = "Hours from End of Auction",
       y = "Cumulative Density",
       title = "Distirbution of Final Bids for Each Bidder")

all_bid_dist <- ld %>%
  ggplot(aes(x = -(hours_from_end-1))) +
  stat_ecdf() +
  theme_bw() +
  scale_x_continuous(trans = reverselog_trans(10),
                     labels=trans_format("identity", function(x) -x + 1),
                     breaks = c(1:7, 9, 1:5 * 10 + 1, 76,  101, 151, 201)) +
  labs(x = "Hours from End of Auction",
       y = "Cumulative Density",
       title = "Distirbution of All Bids")

ggsave(file.path(fdir, "max_bid_dist.png"), max_bid_dist, width = 6, height = 3, units = "in")
ggsave(file.path(fdir, "all_bid_dist.png"), all_bid_dist, width = 6, height = 3, units = "in")

# ==============================================================================
# Difference between live and online ex_ante
# ==============================================================================

size_cdf <- auctions %>%
  ggplot(aes(x=acres, color = online)) +
  stat_ecdf() +
  theme_bw() +
  labs(x = "Tract size (Acres)",
       y = "Cumulative Density",
       color = "Online")

ggsave(file.path(fdir, "size_cdf.png"), size_cdf, width = 6, height = 3, units = "in")

ad <-
  auctions %>%
  group_by(nominator) %>%
  summarise(prop_online = mean(online, na.rm = T),
            nominations = n()) %>%
  mutate(do_both = prop_online > 0 & prop_online < 1)
  
nomination_patterns <- ad %>%
  ggplot(aes(x = nominations, y = prop_online, color = do_both)) +
  geom_jitter() +
  scale_x_log10() +
  geom_smooth(method ="lm") +
  theme_bw() +
  labs(x = "Total Number of Nominations across all Auctions",
       y = "Proportion of Nominations Online",
       color = "Indicator for Participation in Both Types") +
  theme(legend.position = "bottom")

ggsave(file.path(fdir, "nom_patterns.png"), nomination_patterns, width = 6, height = 3.5, units = "in")

a1 <- lm(prop_online ~ log(nominations), data = ad)
a2 <- lm(prop_online ~ log(nominations) * do_both, data = ad)

file_stargazer(output.file = file.path(fdir, "prop_online.tex"),
               a1, a2, omit.stat = c("ser", "F"),
               covariate.labels = c("Log(Total Nominations)", "Participate Both", "Interaction", "Intercept"),
               suppress.errors = TRUE,
               dep.var.labels = c("Proportion of Nominations Online"),
               title = "Regressions testing whether larger firms (those who nominate more parcels) are more likely to nominate in online auctions",
               table.placement = "h!",
               label = "prop_online")

# ==============================================================================
# Participation shifter
# ==============================================================================

pf <- bids %>%
  group_by(lease_id) %>%
  summarise(n_p = length(unique(Bidder))) %>%
  right_join(auctions) %>%
  select(lease_id, n_p, num_adjacent) %>%
  left_join(rd) %>%
  filter(!is.na(n_p)) %>%
  mutate(num_adjacent = case_when(num_adjacent < 1 ~ "0",
                                  num_adjacent < 2 ~ "1",
                                  T ~ "2+"))

shifter_fig <- pf %>%
  ggplot(aes(x = n_p, fill = num_adjacent)) +
  stat_density(alpha = .5, position = "identity", adjust = 1.5) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Number of Bidders",
       fill = "Number Firms with Existing Adjacent Leases",
       y = "Density")

ggsave(file.path(fdir, "shifter_fig.png"), shifter_fig, width = 6, height = 4, units = "in")


p1 = felm(n_p ~ num_adjacent, data = pf)
p2 = felm(log(bonus) ~ num_adjacent | twprngyr, data = pf)
p3 = felm(log(bonus) ~ num_adjacent | twprngyr + n_p, data = pf)
p4 = felm(log(bonus) ~  num_adjacent + P_prov + R_prov + W_prov + royalty_high + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1), data = pf)
p5 = felm(log(bonus) ~  num_adjacent + P_prov + R_prov + W_prov + royalty_high + bs(log(p_back_0), df = 5) + bs(log(mean_peak_gas + 1), df = 3) + log(mean_peak_liq + 1)| n_p, data = pf)


file_stargazer(output.file = file.path(fdir, "shifter_regs.tex"),
               p1, p2, p3, p4, p5, omit.stat = c("ser", "F"),
               covariate.labels = c("1 Adj. Firm", "2+ Adj. Firms"),
               add.lines = list(c("Year * Location FE", "", "Yes", "Yes", "", "", ""),
                                c("Spline Price + Quantity", "", "", "", "Yes", "Yes"),
                                c("Number of Bidders FE", "", "", "Yes", "", "Yes")),
               suppress.errors = TRUE,
               dep.var.labels = c("Number of Bidders", "Log Bonus"),
               title = "Regressions testing whether number of firms with adjacent leases affects log bonus other than by increasing the number of bidders.",
               table.placement = "h!",
               keep = c("num_adjacent"),
               label = "shifter_reg",
               font.size = "footnotesize")

# ==============================================================================
# Make tables showing MLE results for entry and main model
# ==============================================================================

entry_results %>%
  mutate("Model" = paste0(num_adjacent, " Adj. Leases"),
         "$\\lambda$" = paste0(round(Lambda, 2), " (", round(Lambda_Se, 2), ")"),
         "-2 Log Likelihood" = as.character(round(`-2 log Lik`, 2)),
         Observations = as.character(Observations)) %>%
  select(Model, `$\\lambda$`, Observations, `-2 Log Likelihood`) %>%
  pivot_longer(-Model, names_to = " ") %>%
  pivot_wider(names_from = "Model", values_from = value) %>%
  xtable(caption = "Entry Model Estimates", label = "entry_mod") %>%
  print(include.rownames = F, file = file.path(fdir, "entry_mod.tex"),
        sanitize.text.function = function(str) {str})

model_results %>%
  mutate(value = paste0(round(EST, 2), " (", round(SE, 2), ")"),
         "-2 Log Likelihood" = as.character(round(m2ll, 2)),
         Observations = as.character(N)) %>%
  select(Parameter = Paramerer, value, Model, Observations, "-2 Log Likelihood") %>%
  pivot_longer(c(-Parameter, -Model)) %>%
  filter(name == "value" | Parameter == "mu_e") %>%
  mutate(name = if_else(name == "value", Parameter, name)) %>%
  select(-Parameter) %>%
  pivot_wider(names_from = Model) %>%
  arrange(match(name, c("Observations", "-2 Log Likelihood"), nomatch = 0)) %>%
  mutate(name = case_when(name == "mu_e" ~ "$\\mu_\\epsilon$",
                          name == "sig_e" ~ "$\\sigma_\\epsilon$",
                          name == "sig_t" ~ "$\\sigma_\\theta$",
                          TRUE ~ name)) %>%
  rename(" " = name) %>%
  xtable(caption = "Value Model Estimates", label = "value_mod") %>%
  print(include.rownames = F, file = file.path(fdir, "value_mod.tex"),
        sanitize.text.function = function(str) {str})

# ==============================================================================
# Counterfactuals
# ==============================================================================

# set random seed
set.seed(456)
sim_dat = simulation_dat %>% filter(model == "Mod5 no N")

# using fitted data for live auctions,
# simulate a number of bidders N based on entry model, sample that
# many times from the distribution of e, take the max, add to distribution of t

simulate_auction <- function(m) {
  d <-
    counterfactual_df %>%
    ungroup() %>%
    sample_frac(.5, replace = TRUE) %>%
    mutate(N = NA_integer_)
  
  for (l in unique(d$Lambda)) {
    # We fit a truncated distribution in the first place, so I will maintain 
    # that truncated distribution form here
    random_poisson = rpois(5*length(d$lease_id), l*m)
    random_poisson = random_poisson[random_poisson > 0]
    
    d <- d %>%
      mutate(N = if_else(Lambda == l, random_poisson[1:length(.$lease_id)], N))
  }
  sim_resid <- function(N_vec) {
    inner_func <- function(N) {
      if (N == 1) {
        tibble(resid = 0,
               surplus = sample(sim_dat$f_e, 1) +  sample(sim_dat$f_t, 1),
               N_obs = 1)
      } else {
        unsorted = sample(sim_dat$f_e, N)
        current_bid = -10
        bids = 0
        for (bid in unsorted) {
          if (bid > current_bid) {
            current_bid = runif(min = current_bid, max = bid, 1)
            bids = bids + 1
          }
        }
        samp = sort(unsorted)
        tibble(resid = samp[N-1] + sample(sim_dat$f_t, 1),
               surplus = samp[N] - samp[N-1],
               N_obs = bids)
      }
    }
    map_df(N_vec, inner_func)
  }
  d <- d %>%
    bind_cols(sim_resid(.$N)) %>%
    mutate(price = if_else(resid == 0, 0, predicted + resid),
           price = if_else(price < 0 , 0, price),
           surplus = if_else(resid == 0, surplus + predicted, surplus), # single bidders capture lots of surplus
           surplus = if_else(price < 0 , 0, surplus),
           multiplier = paste0("Sim. ", m)) %>%
    select(N, N_obs, price, multiplier, resid, surplus, Lambda)
}

simulate_counterfactuals <- function(...) {
  S = 3 # number of simulations
  rd_cf <- rd %>%
    ungroup() %>%
    sample_frac(.5, replace = TRUE) %>%
    filter(!online, year(auction_date) > 2014)
  
  sims <- map_df(c(rep(1, S),rep(.75, S), rep(.5, S), rep(.4, S), rep(.3, S), rep(.25, S)), simulate_auction)
  
  counterfactual_df %>%
    ungroup() %>%
    sample_frac(.5, replace = TRUE) %>%
    select(price = actual) %>%
    mutate(multiplier = "Actual", N = NA) %>%
    bind_rows(sims) %>%
    group_by(multiplier) %>%
    summarise("Avg Log Price" = mean(price, na.rm = T),
              "Avg N" = mean(N),
              "Avg N Obs" = mean(N_obs, na.rm = T),
              "Avg Surplus" = mean(surplus, na.rm = T),
              "Prop w/ 1 Bid" = mean(N_obs == 1, na.rm = T)) %>%
    mutate(`Prop w/ 1 Bid` = if_else(is.na(`Prop w/ 1 Bid`), mean(rd_cf$hit_min), `Prop w/ 1 Bid`)) %>%
    rename("Experiment" = multiplier) %>% 
    pivot_longer(-Experiment, names_to = "Parameter")
}

policy_experiments <- map_df(1:30, simulate_counterfactuals) %>%
  group_by(Experiment, Parameter) %>%
  summarise(value = paste0(round(mean(value), 2), " (", round(sd(value), 2), ")")) %>%
  mutate(value = if_else(str_detect(value, "NA"), "", value)) %>%
  pivot_wider(names_from = "Parameter")

policy_experiments %>%
  xtable(caption = "Policy experiments where we scale entry parameters of
         poisson process (by the factor listed in the table) to simulate higher
         entry costs in the open outcry auctions during the overlap period
         (2016 and later). The standard errors are boostrapped from multiple
         iterations of the simulation based on different random samples with
         replacement of the open outcry leases going into the simulation.",
         label = "policy_experiments") %>%
  print(include.rownames = F, file = file.path(fdir, "policy_experiments.tex"))


# What level of surplus corresponds to the highest average bidder surplus
# as opposed to winning bidder surplus

sim_avg_surplus <- map_df(rep(10:100, 5)/100, simulate_auction)

surplus_fig <-
  sim_avg_surplus %>%
  mutate(avg_surplus = surplus/N) %>%
  mutate(multiplier = as.numeric(str_remove(multiplier, "Sim.\\s"))) %>%
  group_by(Lambda, multiplier) %>%
  summarise(avg_surplus = mean(avg_surplus)) %>%
  ungroup() %>%
  mutate(Lambda = as.factor(round(Lambda, digits = 2))) %>%
  ggplot(aes(x = multiplier, y = avg_surplus, color = Lambda)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  labs(x = "Participation Multiplier",
       y = "Average Surplus") +
  theme_bw()

ggsave(file.path(fdir, "surplus_fig.png"), surplus_fig,
       width = 6, height = 3, units = "in")
