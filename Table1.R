library(tidyverse) # For data manipulation
library(haven) # reading datasets
library(readr) # For reading datasets
library(sandwich)  # For robust standard errors
library(lmtest)    # For coeftest function

rm(list = ls())

# (1) Replicating Table 1: Summary Statistics ------------------------

# Loading required data
spec_check1572 <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/spec_check1572.rds")
budget1572 <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/budget1572.rds")
consumption_gis <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/consumption_gis.rds")

# GIS grid data
gis_grid <- read_dta("replication_data/gis_grid.dta")

# Elevation ------
thresholds <- c(100, 75, 50, 25)

# Create an empty list to store results
results <- data.frame(
  Threshold = numeric(),
  Intercept = numeric(),
  Pothuan_mita_Coeff = numeric(),
  SE_Diff = numeric(),
  Num_Observations = numeric()
)

# Iterate over each threshold
for (Y in thresholds) {
  # Filter data where d_bnd <= Y
  filtered_data <- gis_grid %>%
    filter(d_bnd <= Y)
  
  # Perform robust regression
  model <- lm(elev ~ pothuan_mita, data = filtered_data)
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))  # Robust standard errors
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  pothuan_mita_coeff <- coef(model)["pothuan_mita"]
  
  # Calculate standard error of the difference
  se_diff <- sqrt(robust_se["(Intercept)"]^2 + robust_se["pothuan_mita"]^2)
  
  # Get number of observations used in the model
  num_observations <- nrow(filtered_data)
  
  # Store results in the data frame
  results <- rbind(results, data.frame(
    Threshold = Y,
    Intercept = intercept,
    Pothuan_mita_Coeff = pothuan_mita_coeff,
    SE_Diff = se_diff,
    Num_Observations = num_observations
  ))
}

# Display the results
print(results)
results <- results %>% mutate(inside = Intercept + Pothuan_mita_Coeff,
                              diff = Intercept - Pothuan_mita_Coeff,
                              t_val = diff/SE_Diff)
results

# Slope --------

# Create an empty list to store results
results <- data.frame(
  Threshold = numeric(),
  Intercept = numeric(),
  Pothuan_mita_Coeff = numeric(),
  SE_Diff = numeric(),
  Num_Observations = numeric()
)

# Iterate over each threshold
for (Y in thresholds) {
  # Filter data where d_bnd <= Y
  filtered_data <- gis_grid %>%
    filter(d_bnd <= Y)
  
  # Perform robust regression
  model <- lm(slope ~ pothuan_mita, data = filtered_data)
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))  # Robust standard errors
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  pothuan_mita_coeff <- coef(model)["pothuan_mita"]
  
  # Calculate standard error of the difference
  se_diff <- sqrt(robust_se["(Intercept)"]^2 + robust_se["pothuan_mita"]^2)
  
  # Get number of observations used in the model
  num_observations <- nrow(filtered_data)
  
  # Store results in the data frame
  results <- rbind(results, data.frame(
    Threshold = Y,
    Intercept = intercept,
    Pothuan_mita_Coeff = pothuan_mita_coeff,
    SE_Diff = se_diff,
    Num_Observations = num_observations
  ))
}

# Display the results
print(results)

results <- results %>% mutate(inside = Intercept + Pothuan_mita_Coeff)
results

# Quechua Speakers ------

# Create an empty list to store results
results <- data.frame(
  Threshold = numeric(),
  Intercept = numeric(),
  Pothuan_mita_Coeff = numeric(),
  SE_Diff = numeric(),
  Num_Observations = numeric()
)

# Iterate over each threshold
for (Y in thresholds) {
  # Filter data where d_bnd <= Y
  filtered_data <- consumption_gis %>%
    filter(d_bnd <= Y & cusco != 1)
  
  # Perform robust regression
  model <- lm(QUE ~ pothuan_mita, data = filtered_data)
  robust_se <- sqrt(diag(vcovHC(model, cluster = filtered_data$ubigeo)))  # Robust standard errors
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  pothuan_mita_coeff <- coef(model)["pothuan_mita"]
  
  # Calculate standard error of the difference
  se_diff <- sqrt(robust_se["(Intercept)"]^2 + robust_se["pothuan_mita"]^2)
  
  # Get number of observations used in the model
  num_observations <- nrow(filtered_data)
  
  # Store results in the data frame
  results <- rbind(results, data.frame(
    Threshold = Y,
    Intercept = intercept,
    Pothuan_mita_Coeff = pothuan_mita_coeff,
    SE_Diff = se_diff,
    Num_Observations = num_observations
  ))
}

# Display the results
print(results)

results <- results %>% mutate(inside = Intercept + Pothuan_mita_Coeff,
                              inside_100 = inside * 100,
                              outside_100 = Intercept * 100)
results


# Tribute 1572 -------
# I leave out this analysis 

# 1572 Budget -------

# Share priest
results <- data.frame(
  Threshold = numeric(),
  Intercept = numeric(),
  Pothuan_mita_Coeff = numeric(),
  SE_Diff = numeric(),
  Num_Observations = numeric()
)

# Iterate over each threshold
for (Y in thresholds) {
  # Filter data where d_bnd <= Y
  filtered_data <- budget1572 %>%
    filter(d_bnd <= Y & cusco != 1)
  
  # Perform robust regression
  model <- lm(sh_priest ~ pothuan_mita, data = filtered_data)
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))  # Robust standard errors
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  pothuan_mita_coeff <- coef(model)["pothuan_mita"]
  
  # Calculate standard error of the difference
  se_diff <- sqrt(robust_se["(Intercept)"]^2 + robust_se["pothuan_mita"]^2)
  
  # Get number of observations used in the model
  num_observations <- nrow(filtered_data)
  
  # Store results in the data frame
  results <- rbind(results, data.frame(
    Threshold = Y,
    Intercept = intercept,
    Pothuan_mita_Coeff = pothuan_mita_coeff,
    SE_Diff = se_diff,
    Num_Observations = num_observations
  ))
}

# Display the results
print(results)

results <- results %>% mutate(inside = Intercept + Pothuan_mita_Coeff,
                              t = Intercept - Pothuan_mita_Coeff/SE_Diff)
results

# Share justice
results <- data.frame(
  Threshold = numeric(),
  Intercept = numeric(),
  Pothuan_mita_Coeff = numeric(),
  SE_Diff = numeric(),
  Num_Observations = numeric()
)

# Iterate over each threshold
for (Y in thresholds) {
  # Filter data where d_bnd <= Y
  filtered_data <- budget1572 %>%
    filter(d_bnd <= Y & cusco != 1)
  
  # Perform robust regression
  model <- lm(sh_justice ~ pothuan_mita, data = filtered_data)
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))  # Robust standard errors
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  pothuan_mita_coeff <- coef(model)["pothuan_mita"]
  
  # Calculate standard error of the difference
  se_diff <- sqrt(robust_se["(Intercept)"]^2 + robust_se["pothuan_mita"]^2)
  
  # Get number of observations used in the model
  num_observations <- nrow(filtered_data)
  
  # Store results in the data frame
  results <- rbind(results, data.frame(
    Threshold = Y,
    Intercept = intercept,
    Pothuan_mita_Coeff = pothuan_mita_coeff,
    SE_Diff = se_diff,
    Num_Observations = num_observations
  ))
}

# Display the results
print(results)

results <- results %>% mutate(inside = Intercept + Pothuan_mita_Coeff,
                              t = Intercept - Pothuan_mita_Coeff/SE_Diff)
results

# share cacique
results <- data.frame(
  Threshold = numeric(),
  Intercept = numeric(),
  Pothuan_mita_Coeff = numeric(),
  SE_Diff = numeric(),
  Num_Observations = numeric()
)

# Iterate over each threshold
for (Y in thresholds) {
  # Filter data where d_bnd <= Y
  filtered_data <- budget1572 %>%
    filter(d_bnd <= Y & cusco != 1)
  
  # Perform robust regression
  model <- lm(sh_cacique ~ pothuan_mita, data = filtered_data)
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))  # Robust standard errors
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  pothuan_mita_coeff <- coef(model)["pothuan_mita"]
  
  # Calculate standard error of the difference
  se_diff <- sqrt(robust_se["(Intercept)"]^2 + robust_se["pothuan_mita"]^2)
  
  # Get number of observations used in the model
  num_observations <- nrow(filtered_data)
  
  # Store results in the data frame
  results <- rbind(results, data.frame(
    Threshold = Y,
    Intercept = intercept,
    Pothuan_mita_Coeff = pothuan_mita_coeff,
    SE_Diff = se_diff,
    Num_Observations = num_observations
  ))
}

# Display the results
print(results)

results <- results %>% mutate(inside = Intercept + Pothuan_mita_Coeff,
                              t = Intercept - Pothuan_mita_Coeff/SE_Diff)
results

# share nobility
results <- data.frame(
  Threshold = numeric(),
  Intercept = numeric(),
  Pothuan_mita_Coeff = numeric(),
  SE_Diff = numeric(),
  Num_Observations = numeric()
)

# Iterate over each threshold
for (Y in thresholds) {
  # Filter data where d_bnd <= Y
  filtered_data <- budget1572 %>%
    filter(d_bnd <= Y & cusco != 1)
  
  # Perform robust regression
  model <- lm(sh_extract ~ pothuan_mita, data = filtered_data)
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))  # Robust standard errors
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  pothuan_mita_coeff <- coef(model)["pothuan_mita"]
  
  # Calculate standard error of the difference
  se_diff <- sqrt(robust_se["(Intercept)"]^2 + robust_se["pothuan_mita"]^2)
  
  # Get number of observations used in the model
  num_observations <- nrow(filtered_data)
  
  # Store results in the data frame
  results <- rbind(results, data.frame(
    Threshold = Y,
    Intercept = intercept,
    Pothuan_mita_Coeff = pothuan_mita_coeff,
    SE_Diff = se_diff,
    Num_Observations = num_observations
  ))
}

# Display the results
print(results)

results <- results %>% mutate(inside = Intercept + Pothuan_mita_Coeff,
                              t = Intercept - Pothuan_mita_Coeff/SE_Diff)
results











