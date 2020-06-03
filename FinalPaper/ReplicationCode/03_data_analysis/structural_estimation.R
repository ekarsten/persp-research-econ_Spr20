# Structural Model of Auction Entry and Valuation
# Eric Karsten ekarsten@uchicago.edu
# April 2020


# Packages
library(tidyverse)
library(lubridate)
library(lfe)
library(splines)
library(fastGHQuad)
library(stats4)

# Repo Setup
root <- getwd()
while(basename(root) != "overlappingauctions") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))
source(file.path(root, "didata.R"))

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

# ==============================================================================
# Load Data
# ==============================================================================

load(file.path(ddir, "clean_data", "auctions.Rda"))
load(file.path(ddir, "clean_data", "bids.Rda"))
load(file.path(ddir, "clean_data", "regression_data.Rda"))

# ==============================================================================
# Step 0: Data Cleaning
# ==============================================================================

# Need to drop auctions with 0 or 1 bidder from this whole thing because they are garbage

rd <-
  regression_data %>%
  select(-max_bid) %>%
  mutate(twprngyr = paste0(twp, rng, year(auction_date)))


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

# ==============================================================================
# Step 1: Control For Observables
# ==============================================================================

c1 = lm(log(bonus) ~ twprngyr + as.factor(n_p), data = pf)
c5 = lm(log(bonus) ~ P_prov + R_prov + W_prov + royalty_high + log(p_back_0) + log(mean_peak_gas + 1) + log(mean_peak_liq + 1) + as.factor(n_p), data = pf)
c1a = lm(log(bonus) ~ twprngyr, data = pf)
c5a = lm(log(bonus) ~ P_prov + R_prov + W_prov + royalty_high + log(p_back_0) + log(mean_peak_gas + 1) + log(mean_peak_liq + 1), data = pf)

# We want to fit the model on residual valuations with the n_p fixed effects added back
structural_df <-
  pf %>%
  mutate(resid1 = log(bonus) - predict(c1, mutate(., n_p = 1)),
         resid5 = log(bonus) - predict(c5, mutate(., n_p = 1)),
         resid1a = log(bonus) - predict(c1a, .),
         resid5a = log(bonus) - predict(c5a, .)) %>%
  select(lease_id, n_p, num_adjacent, resid1, resid5, resid1a, resid5a) %>%
  filter(n_p > 1)

# ==============================================================================
# Step 2: Entry Model
# ==============================================================================


# Simulate P(N_obs| N) for n from 1 to 1000
observed_n = function(N) {
  simulate_entry <- function(...) {
    current_bid = 0
    bids = 0
    for (i in 1:N) {
      shock = runif(1)
      if (shock > current_bid) {
        current_bid = runif(min = current_bid, max = shock, 1)
        bids = bids + 1
      }
    }
    return(bids)
  }
  simulations <- map(1:1e6, simulate_entry)
  
  tibble(n_p = unlist(simulations)) %>%
    group_by(n_p) %>%
    summarise(p_NO_N = n() / 1e6,
              N = N)
}

if (!file.exists(file.path(ddir, "clean_data", "entry_model_probs.Rda"))) {
  entry_model_probs = map_df(1:100, observed_n)
  save(entry_model_probs, file = file.path(ddir, "clean_data", "entry_model_probs.Rda"))
} else {
  load(file.path(ddir, "clean_data", "entry_model_probs.Rda"))
}

lik_data <- 
  structural_df %>%
  left_join(entry_model_probs)

# Solve MLE for negative binomial distribution 
entry_lik <- function(r, p, grp) {
  print(paste(r, p))
  d <- lik_data %>%
    filter(num_adjacent == grp) %>%
    mutate(p_N = 1/(1 - (1 - p)^r - r*p*(1-p)^r) *
             gamma(r + N)/(factorial(N) * gamma(r)) *
             p^N * (1-p)^r,
           p = p_NO_N * p_N) %>%
    group_by(lease_id) %>%
    summarise(p = log(sum(p)))
  out = - sum(d$p)
  print(out)
  if (!is.finite(out) | r < 1) {
    out = 1e6
  }
  return(out)
}

# Solve MLE for poisson distribution 
entry_lik_poisson <- function(lam, grp) {
  d <- lik_data %>%
    filter(num_adjacent == grp) %>%
    mutate(p_N = lam^N * exp(-lam) / ( (1 - exp(-lam) - lam * exp(-lam)) * factorial(N)),
           p = p_NO_N * p_N) %>%
    group_by(lease_id) %>%
    summarise(p = log(sum(p)))
  out = - sum(d$p)
  if (!is.finite(out) | lam < 0) {
    out = 1e6
  }
  return(out)
}

# Function to fit entry parameters for a category of participation shift
fit_entry_mod <- function(grp) {
  obs = sum(structural_df$num_adjacent == grp)
  entry = mle(function(lam) { entry_lik_poisson(lam, grp)},
                start = list(lam = 1),
                nobs = obs) %>%
    summary()
  tibble(
    num_adjacent = grp,
    Lambda = attr(entry, "coef")[1],
    Lambda_Se = attr(entry, "coef")[2],
    "-2 log Lik" = attr(entry, "m2logL"),
    Observations = obs
  )
}

entry_results <- map_df(sort(unique(lik_data$num_adjacent)), fit_entry_mod)

# ==============================================================================
# Step 3: Value Parameterizatoin
# ==============================================================================

# Join r and P parameters on to each auction
# filter to only those actuions with 2 or more bidders
lik_data_2 <-
  structural_df %>%
  left_join(select(entry_results, num_adjacent, Lambda))


# Save fitted values for the offline models
counterfactual_df <-
  auctions %>%
  mutate(num_adjacent = case_when(num_adjacent < 1 ~ "0",
                                  num_adjacent < 2 ~ "1",
                                  T ~ "2+")) %>%
  select(lease_id, num_adjacent) %>%
  left_join(rd) %>%
  filter(online == F) %>%
  filter(year(auction_date) > 2014) %>%
  ungroup() %>%
  mutate(pred5a = predict(c5a, .),
         actual = log(bonus)) %>%
  left_join(select(entry_results, num_adjacent, Lambda)) %>%
  select(lease_id, Lambda, predicted = pred5a, actual)

write_csv(lik_data_2, file.path(ddir, "clean_data", "lik_data_2.csv"))
save(entry_results, file = file.path(ddir, "clean_data", "entry_results.Rda"))
save(counterfactual_df, file = file.path(ddir, "clean_data", "counterfactual_df.Rda"))

# VERY VERY SLOW OPTIMIZATION CODE BELOW. It does work though

# # helper hermite functions
# h_0 = function(z) {1}
# h_1 = function(z) {z}
# h_2 = function(z) {1/sqrt(2) * (z * h_1(z) - h_0(z))}
# h_3 = function(z) {1/sqrt(3) * (z * h_2(z) - sqrt(2)* h_1(z))}
# 
# h = function(z) {c(h_0(z), h_1(z), h_2(z), h_3(z))}
# 
# g_h_weights = gaussHermiteData(10)
# 
# #likelihood
# bid_lik_quadrature <- function(mu_e, sig_e, b_e_1, b_e_2, b_e_3,
#                     sig_t, b_t_1, b_t_2, b_t_3, control_mod) {
#   b_e_0 = sqrt(1 - sum(c(b_e_1, b_e_2, b_e_3)^2))
#   b_t_0 = sqrt(1 - sum(c(b_t_1, b_t_2, b_t_3)^2))
#   b_e = c(b_e_0, b_e_1, b_e_2, b_e_3)
#   b_t = c(b_t_0, b_t_1, b_t_2, b_t_3)
#   
#   func_1 = function(t) {
#     # transform so we are considering e^(-s^2)
#     s = t * sqrt(2)
#     s/(sqrt(2 *pi)) * sum(h(s) * b_t)^2 * sqrt(2)
#   }
#   func_1 <- Vectorize(func_1)
#   mu_t = -sig_t * ghQuad(func_1, g_h_weights)
#   
#   # Helper function
#   f_e <- function(s) {
#     1/sqrt(2*pi*sig_e^2) * sum(h((s - mu_e)/sig_e) * b_e)^2 * exp(-.5 * ((s - mu_e)/sig_e)^2)
#   }
#   f_e <- Vectorize(f_e)
# 
#   
#   F_e <- function(s) {
#     integrate(f_e, -Inf, s, stop.on.error = F)$value
#   }
#   F_e <- Vectorize(F_e)
#   
#   H_e <- function(s) {
#     sum(h((s - mu_e)/sig_e) * b_e)^2
#   }
#   H_e <- Vectorize(H_e)
#   
#   H_t <- function(s) {
#     sum(h((s - mu_t)/sig_t) * b_t)^2
#   }
#   H_t <- Vectorize(H_t)
#   
#   f_transact <- function(t, lam) {
#     A <- (lam^2 *exp(-lam))/(1 - exp(-lam) - lam * exp(-lam)) *
#       (sqrt(2) * sig_t * sig_e)/(sqrt(sig_t^2 + sig_e^2)) *
#       1/(2 * pi * sqrt(sig_e * sig_t)) *
#       exp(- (t - mu_t + mu_e)^2 /(2 * (sig_t^2 + sig_e^2)))
#     inner <- function(u) {
#       s <- (sqrt(2) * sig_t * sig_e)/(sqrt(sig_t^2 + sig_e^2)) * u +
#         (sig_e^2 * (t - mu_t) + sig_t^2 * mu_e)/(sig_t^2 + sig_e^2)
#       
#       F_e_s <- F_e(s)
#       G <- (1 - F_e_s) * exp(lam * F_e_s)
#       H <- H_e(s) * H_t(t - s)
#       G*H
#     }
#     B <- ghQuad(inner, g_h_weights)
#     A*B
#   }
#   f_transact <- Vectorize(f_transact)
#   
#   if (control_mod == "1" & !is.na(b_e_0) & !is.na(b_t_0)) {
#     out = f_transact(lik_data_2$resid1, lik_data_2$Lambda)
#   } else if (control_mod == "5" & !is.na(b_e_0) & !is.na(b_t_0)) {
#     out = f_transact(lik_data_2$resid5, lik_data_2$Lambda)
#   } else {
#     out = NA
#   }
#   out = - sum(log(out))
#   print(out)
#   if (!is.finite(out) ) {
#     out = 1e10
#   }
#   return(out)
# }
# 
# value_mod <- mle(function(mu_e, sig_e, b_e_1, b_e_2, b_e_3,
#                          sig_t, b_t_1, b_t_2, b_t_3) {
#     bid_lik_quadrature(mu_e, sig_e, b_e_1, b_e_2, b_e_3, sig_t,
#                        b_t_1, b_t_2, b_t_3, control_mod = "1")},
#   start = list(mu_e = 0, sig_e = 1, b_e_1 = .5 , b_e_2 = .5, b_e_3 = .5,
#                sig_t = 1, b_t_1 = .5, b_t_2 = .5, b_t_3 = .5),
#   nobs = length(!is.na(lik_data_2$resid1)),
#   method = "L-BFGS-B",
#   lower = c(-5, .001, .001, .001, .001, .001, .001, .001, .001),
#   upper = c( 5, 5, 1, 1, 1, 5, 1, 1, 1)) %>%
#   summary()
# 
# 
# 
