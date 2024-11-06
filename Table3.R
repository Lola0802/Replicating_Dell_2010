library(tidyverse)
library(haven)
library(readr)
library(lmtest)
library(sandwich)

# Replicating Table 3: Robustness tests for RDD polynomial ------------------------
rm(list = ls())

# Data files used:
height <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/height.rds")
consumption <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/consumption.rds")
gis_dist <- read_dta("replication_data/gis_dist.dta")

# (1) CONSUMPTION REGRESSIONS ----------
data1 <- left_join(gis_dist, consumption, by = "ubigeo") %>% filter(!is.na(hhead))
saveRDS(data1, "output/consumption_gis.rds")

consumption_gis <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/consumption_gis.rds")
# (1.1) Alternative Functional Forms for RD Polynomial: Baseline I 
# a. Linear polynomial in latitude and longitude
# b. Quadratic polynomial in latitude and longitude
# c. Quartic polynomial in latitude and longitude

# (1.2) Alternative Functional Forms for RD Polynomial: Baseline II
# a. Linear polynomial in distance to Potosi ####
# Distance < 100 km
model <- lm(lhhequiv ~ pothuan_mita + dpot + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 100))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Distance < 75km
model <- lm(lhhequiv ~ pothuan_mita + dpot + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 75))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Distance < 50
model <- lm(lhhequiv ~ pothuan_mita + dpot + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 50))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# b. Quadratic polynomial in distance to Potosi ####
# Distance < 100 km
model <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 100))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Distance < 75km
model <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 75))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Distance < 50
model <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2+ infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 50))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# c. Quartic polynomial in distance to Potosi ####
# Distance < 100 km
model <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2 + dpot3 + dpot4 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 100))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Distance < 75km
model <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2 + dpot3 + dpot4 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 75))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Distance < 50
model <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2 + dpot3 + dpot4 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 50))

cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# d. Interacted linear polynomial in distance to Potosi ####

# Distance < 100 km
model <- lm(lhhequiv ~ pothuan_mita + dpot + pothuan_mita * dpot + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 100))

# Calculate the mean of dpot for observations where d_bnd < 10, which will stay the same for following iterations
mean_dpot <- consumption_gis %>% filter(d_bnd < 10) %>% summarize(mean_dpot = mean(dpot, na.rm = TRUE)) %>% pull(mean_dpot)

# Extract coefficients
coef_pothuan_mita <- coef(model)["pothuan_mita"]
coef_interaction <- coef(model)["pothuan_mita:dpot"]

# Calculate the nonlinear combination
marginal_effect <- coef_pothuan_mita + coef_interaction * mean_dpot

# Extract variances and covariance from the covariance matrix
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
var_pothuan_mita <- cluster_se["pothuan_mita", "pothuan_mita"]
var_interaction <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot"]
cov_pothuan_mita_interaction <- cluster_se["pothuan_mita", "pothuan_mita:dpot"]

# Calculate the standard error of the marginal effect
se_marginal_effect <- sqrt(var_pothuan_mita + (mean_dpot^2) * var_interaction + 2 * mean_dpot * cov_pothuan_mita_interaction)

# Display the marginal effect and its standard error
marginal_effect
se_marginal_effect

# Compute the t-value and p-value
t_value <- marginal_effect / se_marginal_effect
p_value <- 2 * (1 - pnorm(abs(t_value)))  # Two-tailed test

# Determine significance stars
significance_stars <- ifelse(p_value < 0.001, "***",
                             ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
significance_stars

# Distance < 75km
model <- lm(lhhequiv ~ pothuan_mita + dpot + pothuan_mita * dpot + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 75))

# Calculate the mean of dpot for observations where d_bnd < 10, which will stay the same for following iterations
mean_dpot <- consumption_gis %>% filter(d_bnd < 10) %>% summarize(mean_dpot = mean(dpot, na.rm = TRUE)) %>% pull(mean_dpot)

# Extract coefficients
coef_pothuan_mita <- coef(model)["pothuan_mita"]
coef_interaction <- coef(model)["pothuan_mita:dpot"]

# Calculate the nonlinear combination
marginal_effect <- coef_pothuan_mita + coef_interaction * mean_dpot

# Extract variances and covariance from the covariance matrix
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
var_pothuan_mita <- cluster_se["pothuan_mita", "pothuan_mita"]
var_interaction <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot"]
cov_pothuan_mita_interaction <- cluster_se["pothuan_mita", "pothuan_mita:dpot"]

# Calculate the standard error of the marginal effect
se_marginal_effect <- sqrt(var_pothuan_mita + (mean_dpot^2) * var_interaction + 2 * mean_dpot * cov_pothuan_mita_interaction)

# Display the marginal effect and its standard error
marginal_effect
se_marginal_effect

# Compute the t-value and p-value
t_value <- marginal_effect / se_marginal_effect
p_value <- 2 * (1 - pnorm(abs(t_value)))  # Two-tailed test

# Determine significance stars
significance_stars <- ifelse(p_value < 0.001, "***",
                             ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
significance_stars

# Distance < 50
model <- lm(lhhequiv ~ pothuan_mita + dpot + pothuan_mita * dpot + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 50))

# Calculate the mean of dpot for observations where d_bnd < 10, which will stay the same for following iterations
mean_dpot <- consumption_gis %>% filter(d_bnd < 10) %>% summarize(mean_dpot = mean(dpot, na.rm = TRUE)) %>% pull(mean_dpot)

# Extract coefficients
coef_pothuan_mita <- coef(model)["pothuan_mita"]
coef_interaction <- coef(model)["pothuan_mita:dpot"]

# Calculate the nonlinear combination
marginal_effect <- coef_pothuan_mita + coef_interaction * mean_dpot

# Extract variances and covariance from the covariance matrix
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
var_pothuan_mita <- cluster_se["pothuan_mita", "pothuan_mita"]
var_interaction <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot"]
cov_pothuan_mita_interaction <- cluster_se["pothuan_mita", "pothuan_mita:dpot"]

# Calculate the standard error of the marginal effect
se_marginal_effect <- sqrt(var_pothuan_mita + (mean_dpot^2) * var_interaction + 2 * mean_dpot * cov_pothuan_mita_interaction)

# Display the marginal effect and its standard error
marginal_effect
se_marginal_effect

# Compute the t-value and p-value
t_value <- marginal_effect / se_marginal_effect
p_value <- 2 * (1 - pnorm(abs(t_value)))  # Two-tailed test

# Determine significance stars
significance_stars <- ifelse(p_value < 0.001, "***",
                             ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
significance_stars

# e. Interacted quadratic polynomial in distance to Potosi ####
# Distance < 100 
model <- lm(lhhequiv ~ pothuan_mita + dpot + pothuan_mita * dpot + dpot2 + pothuan_mita * dpot2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 100))

# Calculate the mean of dpot for observations where d_bnd < 10, which will stay the same for following iterations
mean_dpot <- consumption_gis %>% filter(d_bnd < 10) %>% summarize(mean_dpot = mean(dpot, na.rm = TRUE)) %>% pull(mean_dpot)

# Extract coefficients
coef_pothuan_mita <- coef(model)["pothuan_mita"]
coef_interaction_dpot <- coef(model)["pothuan_mita:dpot"]
coef_interaction_dpot2 <- coef(model)["pothuan_mita:dpot2"]

# Extract variances and covariance from the covariance matrix
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
var_pothuan_mita <- cluster_se["pothuan_mita", "pothuan_mita"]
var_interaction_dpot <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot"]
var_interaction_dpot2 <- cluster_se["pothuan_mita:dpot2", "pothuan_mita:dpot2"]

cov_pothuan_mita_interaction_dpot <- cluster_se["pothuan_mita", "pothuan_mita:dpot"]
cov_pothuan_mita_interaction_dpot2 <- cluster_se["pothuan_mita", "pothuan_mita:dpot2"]
cov_interaction_dpot_interaction_dpot2 <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot2"]

# Calculate the marginal effect
marginal_effect <- coef_pothuan_mita + coef_interaction_dpot * mean_dpot + coef_interaction_dpot2 * mean_dpot^2

# Calculate the standard error of the marginal effect
se_marginal_effect <- sqrt(
  var_pothuan_mita +
  mean_dpot^2 * var_interaction_dpot +
  mean_dpot^4 * var_interaction_dpot2 +
  2 * mean_dpot * cov_pothuan_mita_interaction_dpot +
  2 * mean_dpot^2 * cov_pothuan_mita_interaction_dpot2 +
  2 * mean_dpot^3 * cov_interaction_dpot_interaction_dpot2
)

# Display the marginal effect and its standard error
marginal_effect
se_marginal_effect

# NOTE: If se comes out NaN, please uncomment and run: 
# # Check if any of the variables are NaN or Inf
# any(is.nan(c(var_pothuan_mita, var_interaction_dpot, var_interaction_dpot2,
#              cov_pothuan_mita_interaction_dpot, cov_pothuan_mita_interaction_dpot2,
#              cov_interaction_dpot_interaction_dpot2)))
# 
# any(is.infinite(c(var_pothuan_mita, var_interaction_dpot, var_interaction_dpot2,
#                   cov_pothuan_mita_interaction_dpot, cov_pothuan_mita_interaction_dpot2,
#                   cov_interaction_dpot_interaction_dpot2)))
# # These should be FALSE
# 
# # Then run this code, it always works
# # Calculate each term separately
# term1 <- var_pothuan_mita
# term2 <- mean_dpot^2 * var_interaction_dpot
# term3 <- mean_dpot^4 * var_interaction_dpot2
# term4 <- 2 * mean_dpot * cov_pothuan_mita_interaction_dpot
# term5 <- 2 * mean_dpot^2 * cov_pothuan_mita_interaction_dpot2
# term6 <- 2 * mean_dpot^3 * cov_interaction_dpot_interaction_dpot2
# 
# # Print each term
# print(c(term1, term2, term3, term4, term5, term6))
# 
# # Calculate the standard error
# se_marginal_effect <- sqrt(term1 + term2 + term3 + term4 + term5 + term6)
# print(se_marginal_effect)


# Compute the t-value and p-value
t_value <- marginal_effect / se_marginal_effect
p_value <- 2 * (1 - pnorm(abs(t_value)))  # Two-tailed test

# Determine significance stars
significance_stars <- ifelse(p_value < 0.001, "***",
                             ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
significance_stars

# Distance < 75

model <- lm(lhhequiv ~ pothuan_mita + dpot + pothuan_mita * dpot + dpot2 + pothuan_mita * dpot2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 75))

# Calculate the mean of dpot for observations where d_bnd < 10, which will stay the same for following iterations
mean_dpot <- consumption_gis %>% filter(d_bnd < 10) %>% summarize(mean_dpot = mean(dpot, na.rm = TRUE)) %>% pull(mean_dpot)

# Extract coefficients
coef_pothuan_mita <- coef(model)["pothuan_mita"]
coef_interaction_dpot <- coef(model)["pothuan_mita:dpot"]
coef_interaction_dpot2 <- coef(model)["pothuan_mita:dpot2"]

# Extract variances and covariance from the covariance matrix
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
var_pothuan_mita <- cluster_se["pothuan_mita", "pothuan_mita"]
var_interaction_dpot <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot"]
var_interaction_dpot2 <- cluster_se["pothuan_mita:dpot2", "pothuan_mita:dpot2"]

cov_pothuan_mita_interaction_dpot <- cluster_se["pothuan_mita", "pothuan_mita:dpot"]
cov_pothuan_mita_interaction_dpot2 <- cluster_se["pothuan_mita", "pothuan_mita:dpot2"]
cov_interaction_dpot_interaction_dpot2 <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot2"]

# Calculate the marginal effect
marginal_effect <- coef_pothuan_mita + coef_interaction_dpot * mean_dpot + coef_interaction_dpot2 * mean_dpot^2

# Calculate the standard error of the marginal effect
se_marginal_effect <- sqrt(
  var_pothuan_mita +
  mean_dpot^2 * var_interaction_dpot +
  mean_dpot^4 * var_interaction_dpot2 +
  2 * mean_dpot * cov_pothuan_mita_interaction_dpot +
  2 * mean_dpot^2 * cov_pothuan_mita_interaction_dpot2 +
  2 * mean_dpot^3 * cov_interaction_dpot_interaction_dpot2
)

# Display the marginal effect and its standard error
marginal_effect
se_marginal_effect

# Compute the t-value and p-value
t_value <- marginal_effect / se_marginal_effect
p_value <- 2 * (1 - pnorm(abs(t_value)))  # Two-tailed test

# Determine significance stars
significance_stars <- ifelse(p_value < 0.001, "***",
                             ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
significance_stars

# Distance < 50
model <- lm(lhhequiv ~ pothuan_mita + dpot + pothuan_mita * dpot + dpot2 + pothuan_mita * dpot2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = consumption_gis %>% filter(cusco != 1 & d_bnd < 50))

# Calculate the mean of dpot for observations where d_bnd < 10, which will stay the same for following iterations
mean_dpot <- consumption_gis %>% filter(d_bnd < 10) %>% summarize(mean_dpot = mean(dpot, na.rm = TRUE)) %>% pull(mean_dpot)

# Extract coefficients
coef_pothuan_mita <- coef(model)["pothuan_mita"]
coef_interaction_dpot <- coef(model)["pothuan_mita:dpot"]
coef_interaction_dpot2 <- coef(model)["pothuan_mita:dpot2"]

# Extract variances and covariance from the covariance matrix
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
var_pothuan_mita <- cluster_se["pothuan_mita", "pothuan_mita"]
var_interaction_dpot <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot"]
var_interaction_dpot2 <- cluster_se["pothuan_mita:dpot2", "pothuan_mita:dpot2"]

cov_pothuan_mita_interaction_dpot <- cluster_se["pothuan_mita", "pothuan_mita:dpot"]
cov_pothuan_mita_interaction_dpot2 <- cluster_se["pothuan_mita", "pothuan_mita:dpot2"]
cov_interaction_dpot_interaction_dpot2 <- cluster_se["pothuan_mita:dpot", "pothuan_mita:dpot2"]

# Calculate the marginal effect
marginal_effect <- coef_pothuan_mita + coef_interaction_dpot * mean_dpot + coef_interaction_dpot2 * mean_dpot^2

# Calculate the standard error of the marginal effect
se_marginal_effect <- sqrt(
  var_pothuan_mita +
  mean_dpot^2 * var_interaction_dpot +
  mean_dpot^4 * var_interaction_dpot2 +
  2 * mean_dpot * cov_pothuan_mita_interaction_dpot +
  2 * mean_dpot^2 * cov_pothuan_mita_interaction_dpot2 +
  2 * mean_dpot^3 * cov_interaction_dpot_interaction_dpot2
)

# Display the marginal effect and its standard error
marginal_effect
se_marginal_effect

# Compute the t-value and p-value
t_value <- marginal_effect / se_marginal_effect
p_value <- 2 * (1 - pnorm(abs(t_value)))  # Two-tailed test

# Determine significance stars
significance_stars <- ifelse(p_value < 0.001, "***",
                             ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
significance_stars


# (1.3) Alternative Functional Forms for RD Polynomial: Baseline III
# a. Linear polynomial in distance to mita boundary
# b. Quadratic polynomial in distance to mita boundary
# c. Quartic polynomial in distance to mita boundary
# d. Interacted linear polynomial in distance to mita boundary
# e. Interacted quadratic polynomial in distance to mita boundary

# (1.4) Ordinary Least Squares 
