# (1) Replicating Table 5: Summary Statistics ------------------------

# If the RD specification is estimating the mita’s long-run effect as opposed to some other underlying difference, being inside the mita catchment should not affect economic prosperity, institutions, or demographics prior to the mita’s enactment.

rm(list = ls())

options(scipen = 999)

library(tidyverse)
library(haven)
library(readr)
library(labelled)
library(lmtest)
library(sandwich)

# Input files:
gis_dist <- read_dta("replication_data/gis_dist.dta")
budget1572 <- read_csv("replication_data/1572budget.csv")
demographic1572 <- read_csv("replication_data/1572demographic.csv")
tribute1572 <- read_dta("replication_data/tribute1572.dta")

# Data Files Created as Final Product
	# spec_check1572.rds
	# budget1572.rds
	# dem1572.rds

# (1) Mean Per Capita Tribune --------

# Assigning labels
# Load the labelled package
library(labelled)

# Assuming your data frame is called `data`

# Set variable labels
tribute1572 <- set_variable_labels(
  tribute1572,
  poblado = "center populated",
  eid = "encomienda id",
  ccdd = "department",
  ccpp = "province",
  ccdi = "district",
  trib_pop = "Tributary Pop.",
  oro = "Gold",
  plata = "Silver",
  ropa_abasca = "Clothing from abasca",
  carnero = "Land sheep",
  ropa_algadon = "Cotton clothing",
  chuno = "Chuno",
  maiz = "Corn",
  hechura = "Craftsmanship",
  aves = "Birds",
  pescado = "Dried fish",
  costales = "Sacks",
  papas = "Potatoes",
  pacos = "Pacos",
  trigo = "Wheat",
  ropa_cumbi = "Clothing from cumbi",
  puercos = "Pigs",
  alpargatas = "Espadrilles",
  cebada = "Barley",
  sal = "Salt",
  yana = "Yanacona"
)

# Convert numerical variables to integer (byte equivalent)
tribute1572 <- tribute1572 %>%
  mutate(
    ccdd = as.integer(ccdd),
    ccpp = as.integer(ccpp),
    ccdi = as.integer(ccdi)
  )

# Recode missing values to zero
tribute1572 <- tribute1572 %>%
  mutate(
    oro = replace_na(oro, 0),
    plata = replace_na(plata, 0),
    ropa_abasca = replace_na(ropa_abasca, 0),
    carnero = replace_na(carnero, 0),
    ropa_algadon = replace_na(ropa_algadon, 0),
    chuno = replace_na(chuno, 0),
    maiz = replace_na(maiz, 0),
    hechura = replace_na(hechura, 0),
    aves = replace_na(aves, 0),
    pescado = replace_na(pescado, 0),
    costales = replace_na(costales, 0),
    papas = replace_na(papas, 0),
    pacos = replace_na(pacos, 0),
    trigo = replace_na(trigo, 0),
    ropa_cumbi = replace_na(ropa_cumbi, 0),
    puercos = replace_na(puercos, 0),
    alpargatas = replace_na(alpargatas, 0),
    cebada = replace_na(cebada, 0),
    sal = replace_na(sal, 0)
  )

tribute1572 <- tribute1572 %>% # Calculate total tribute
  mutate(trib_tot = oro + plata + ropa_abasca + carnero + ropa_algadon + chuno + maiz + hechura + aves + pescado + costales + papas + pacos + trigo + ropa_cumbi + puercos + alpargatas + cebada + sal) %>% 
  filter(yana != 1) 

# Grouping at the ccdd, ccpp, ccdi level - geographic level
agg_tribute <- tribute1572 %>% group_by(ccdd, ccpp, ccdi) %>%
  summarise(across(c(trib_pop, oro, plata, ropa_abasca, carnero, ropa_algadon, chuno, maiz, hechura, aves, pescado, costales, papas, pacos, trigo, ropa_cumbi, puercos, alpargatas, cebada, sal, trib_tot, match_pcap), sum, na.rm = TRUE))

# Calculating tribute rates
agg_tribute <- agg_tribute  %>% 
  mutate(trib_rate = trib_tot / trib_pop) %>% # calculate the tribute rate and take its log
  mutate(ltrib_rate = log(trib_rate)) %>% mutate(ubigeo = ccdi + 100 * ccpp + 10000 * ccdd)

# Merging with GIS data using ubigeo
spec_check1572 <- inner_join(gis_dist, agg_tribute, by = "ubigeo")

saveRDS(spec_check1572, "output/spec_check1572.rds")

# Regressions:
# To do weighted regressions, I use the survey package. Weighted regressions are not as straightforward in R as they are in Stata, so I expect very different answers. 

library(survey)

# Cubic polynomial in latitude and longitude
# Load and filter the data
data <- spec_check1572 %>% filter(cusco != 1 & d_bnd < 50)

# Define the weights
weights <- data$trib_pop

# Fit the model with weights
model <- svyglm(ltrib_rate ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3,
                 design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())

# Display the summary of the model
summary(model) # not using clustered standard errors

# Cubic polynomial in distance to Potosi
model <- svyglm(ltrib_rate ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
                design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())

# Display the summary of the model
summary(model)

# Cubic polynomial in distance to the mita boundary
# Weighted mean of ltrib_pop
weighted_mean <- weighted.mean(data$ltrib_rate, weights)

# Create survey design
design <- svydesign(ids = ~1, weights = ~weights, data = data)

# Fit the weighted regression model
model <- svyglm(ltrib_rate ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3,
                 design = design)

# Display the summary of the model
summary(model)

# (2) Budget -------
budget1572$construct_church <- as.numeric(budget1572$construct_church)

budget1572 <- budget1572 %>% 
  mutate(across(c(priest, justice, cacique, construct_church, hospital), 
                ~ as.numeric(ifelse(. == ".", 0, .))))

agg_budget <- budget1572 %>% 
  group_by(ccdd, ccpp, ccdi) %>%
  summarise(
    priest = sum(priest, na.rm = TRUE),
    justice = sum(justice, na.rm = TRUE),
    cacique = sum(cacique, na.rm = TRUE),
    construct_church = sum(construct_church, na.rm = TRUE),
    hospital = sum(hospital, na.rm = TRUE)
  ) %>%
  mutate(admin = sum(priest, justice, cacique, construct_church, hospital), na.rm = TRUE) # Create `admin` as sum

# Merging budget and tribute data
budget_tribute <- spec_check1572 %>% filter(d_bnd < 100) %>% 
  inner_join(agg_budget, by = c("ccdd", "ccpp", "ccdi")) %>% 
  filter(!is.na(admin)) %>% # Keep only non-missing values in 'admin'
  mutate(extraction = trib_tot - admin) %>% # Create new variable extraction
  mutate(sh_priest = priest / trib_tot, # Generate share variables
         sh_justice = justice / trib_tot,
         sh_cacique = cacique / trib_tot,
         sh_extract = extraction / trib_tot)

saveRDS(budget_tribute, "output/budget1572.rds")
# Regressions
# Cubic polynomial in latitude and longitude

# share of nobility
model <- lm(sh_extract ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Share of spanish priests
model <- lm(sh_priest ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Spanish Justices
model <- lm(sh_justice ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Indigeneous mayors
model <- lm(sh_cacique ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Single dimensional RD polynomials

# Distance to Potosi

# share of nobility
model <- lm(sh_extract ~ pothuan_mita + dpot + dpot2 + dpot3
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Share of spanish priests
model <- lm(sh_priest ~ pothuan_mita + dpot + dpot2 + dpot3
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Spanish Justices
model <- lm(sh_justice ~ pothuan_mita + dpot + dpot2 + dpot3
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Indigeneous mayors
model <- lm(sh_cacique ~ pothuan_mita + dpot + dpot2 + dpot3
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Distance to mita boundary 

# share of nobility
model <- lm(sh_extract ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Share of spanish priests
model <- lm(sh_priest ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Spanish Justices
model <- lm(sh_justice ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# Indigeneous mayors
model <- lm(sh_cacique ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3
            + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
            data = budget_tribute %>% filter(cusco != 1 & d_bnd< 50))

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se <- vcovCL(model, cluster = ~ ubigeo)
summary <- summary(model, robust = cluster_se)
summary

# (3) Demographics -----

demographic1572 <- demographic1572 %>% 
  mutate(across(c(old_men, boys, women, tot_other), 
                ~ as.numeric(ifelse(. == ".", 0, .))))

agg_demo <- demographic1572 %>% 
  group_by(ccdd, ccpp, ccdi) %>%
  summarise(
    trib_pop = sum(trib_pop, na.rm = TRUE),
    old_men = sum(old_men, na.rm = TRUE),
    boys = sum(boys, na.rm = TRUE),
    women = sum(women, na.rm = TRUE),
    tot_other = sum(tot_other, na.rm = TRUE)
  )

agg_demo <- agg_demo %>% 
  mutate(total_pop = trib_pop + old_men + boys + women,
         total_pop = if_else(is.na(total_pop), trib_pop, total_pop),
         sh_trib = trib_pop / total_pop,
         sh_oldmen = old_men / total_pop,
         sh_boys = boys / total_pop,
         sh_women = women / total_pop)

demo_tribute <- spec_check1572 %>% filter(d_bnd < 100) %>% 
  select(pothuan_mita, elv_sh, slope, dpot, dpot2, dbnd_sh, dbnd_sh2, starts_with("bfe4"), 
         cusco, ubigeo, d_bnd, ccdd, ccpp, ccdi, starts_with("x"), starts_with("y")) %>% 
  left_join(agg_demo, by = c("ccdd", "ccpp", "ccdi"))

saveRDS(demo_tribute, "output/demographic1572.rds")


# Regressions #

# Cubic polynomial in latitude and longitude

# Share of men
# Load and filter the data
data <- demo_tribute %>% filter(cusco != 1 & d_bnd < 50)

# Define the weights
weights <- data$total_pop

# Fit the model with weights
model <- svyglm(sh_trib ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3,
                 design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())

# Display the summary of the model
summary(model) # not using clustered standard errors

# Share of boys
model <- svyglm(sh_boys ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3,
                 design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())
summary(model)
# Share of females 
model <- svyglm(sh_women ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3,
                 design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())
summary(model)

# Cubic polynomial in distance to Potosi
data <- data %>% mutate(dpot3 = dpot^3, dbnd_sh3 = dbnd_sh^3)

# Men
model <- svyglm(sh_trib ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
                design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())
summary(model)

# Boys
model <- svyglm(sh_boys ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
                design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())
summary(model)
# Women
model <- svyglm(sh_women ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, 
                design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())
summary(model)

# Cubic polynomial in distance to the mita boundary
# Men
model <- svyglm(sh_trib ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3, design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())
summary(model)

# Boys
model <- svyglm(sh_boys ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3, design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())
summary(model)

# Women
model <- svyglm(sh_women ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3, design = svydesign(ids = ~1, weights = ~weights, data = data),
                 family = gaussian())
summary(model)


# All the regression outputs were collected manually and input into a table



