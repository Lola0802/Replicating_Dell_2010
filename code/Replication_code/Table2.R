options(scipen = 999)

library(tidyverse)
library(haven)
library(readr)
library(labelled)
library(lmtest)
library(sandwich)
library(stargazer)

# (2) Replicating Table 2: Main Results ------------------------
# Table 2 shows the results from estimating the mita’s impact on living standards today, through household consumption (using the log of equivalent household consumption, net transfers, in 2001 as the dependent variable) and through childhood stunting (using a dummy equal to 1 if the child’s growth is stunted and equal to 0 otherwise as the dependent variable).

# (2.0) Creating files required for analysis ------

# Loading data
# Consumption data
# Height data
# ctalla.dta
# # GIS data
# gis_data <- read_dta("replication_data/gis_dist.dta")

# (2.0.1) Household data -------
household_data <- read_dta("replication_data/enaho01_2001iv_200.dta")

# (2.0.1.1) Clean household data ------
household_data <- household_data %>%
select(conglome, vivienda, hogar, ubigeo, p203, p207, p208a, p208b) %>% # Get household and identifier codes and data on age and sex of members
filter(p203 != 0) %>% # Filter out people who were in the panel before but not in household anymore
mutate(
	p208b = ifelse(is.na(p208b), 0, p208b),
	age = p208a + (p208b / 12),
	hhead = if_else(p203 >= 2 & p203 <= 10, 0, p203),
	male = if_else(p207 == 2, 0, p207)
	) %>%
select(-p208a, -p208b, -p203, -p207 ) # TODO: Remove this comment later: 75,477 entries, 7 total columns, 7 NAs

# Age frequency distribution
View(household_data %>% group_by(age) %>% summarise(n = n())) # 7 NA values, remove 

household_data <- household_data %>% filter(!is.na(age)) # 75470 entries

# Frequency distribution of hhead grouped by conglomerate, housing & home
View(household_data %>% group_by(conglome, vivienda, hogar) %>% summarise(test = sum(hhead)))

# Labelling all variables:
household_data <- household_data %>%
set_variable_labels(
	conglome = "cluster",
	vivienda = "dwelling",
	hogar = "household",
	ubigeo = "geographical location",
	hhead = "household head",
	male = "male"
	)
# Label definition for hhead and male
household_data$hhead <- labelled(household_data$hhead, labels = c("no" = 0, "yes" = 1))
household_data$male <- labelled(household_data$male, labels = c("female" = 0, "male" = 1))

look_for(household_data) # checking labels

# Extract geographical codes and keep only the departments in sample
household_data1 <- household_data %>%
mutate(
	ccdd = substr(ubigeo, 1, 2), # ccdd gets the first 2 characters of ubigeo (representing the department).
	ccpp = substr(ubigeo, 3, 4), # ccpp gets the next 2 characters of ubigeo (representing the province).
	ccdi = substr(ubigeo, 5, 6) # ccdi gets the final 2 characters of ubigeo (representing the district).
	) %>%
mutate_at(vars(ccdd, ccpp, ccdi), as.numeric) %>% # convert character to numeric
filter(ccdd %in% c(3, 4, 5, 8, 21))  # 13130 obs 
# remove departments not in sample

# (2.0.1.2) Create dependent variable, household consumption net transfers per equivalent member ------

household_data1 <- household_data1 %>%
mutate(hid = paste0(conglome, vivienda, hogar), # Generate a unique household identifier
	var1 = 1)  %>% # This is a counter for the next step
group_by(hid) %>% # hid is combining strings which sometimes have empty values i.e. " 021". Check if this causes problems later, but it shouldn't
mutate(hhmem = sum(var1),# create a variable which contains the number of members per household
	lhhmem = log(hhmem)) %>% # create log of hhmem
ungroup() %>% select(-var1)

# Assigning labels to variables
var_label(household_data1$hid) <- "household identifier"
var_label(household_data1$hhmem) <- "number of household members"
var_label(household_data1$lhhmem) <- "log number of household members"

# Creating dummy for kids, calculating kids/hh member

# Recode age categories
household_data1 <- household_data1 %>%
mutate(age_cat = case_when(
	age >= 0 & age <= 14 ~ 1,
	age >= 15 ~ 2,
	TRUE ~ NA_real_
	))

# Create indicator variables
household_data1 <- household_data1 %>%
mutate(ac1 = ifelse(age_cat == 1, 1, 0),
	ac2 = ifelse(age_cat == 2, 1, 0))

# Calculate number of children per household
data_summary <- household_data1 %>%
group_by(hid) %>%
summarise(kids = sum(ac1, na.rm = TRUE))

# Drop temporary indicator variables
household_data1 <- household_data1 %>%
select(-ac1, -ac2)

# Display frequency table of kids
table(data_summary$kids)

# Merge with household member data and calculate ratio
data_summary <- data_summary %>%
left_join(household_data1 %>%
	group_by(hid) %>%
	summarise(hhmem = unique(hhmem)),
	by = "hid") %>%
mutate(k_hhmem = kids / hhmem)

# combining datasets
household_data1 <- left_join(household_data1, data_summary, by = c("hid" , "hhmem")) %>% select(-age_cat)

# Coding Deaton values and calculating consumption equivalent scale

# Step 1: Create age categories
household_data1 <- household_data1 %>%
mutate(age_cat = case_when(
	age >= 0 & age <= 4 ~ 1,
	age >= 5 & age <= 14 ~ 2,
	age >= 15 ~ 3,
	TRUE ~ NA_real_
	))

# Step 2: Create indicator (dummy) variables for age categories
household_data1 <- household_data1 %>%
mutate(ac1 = ifelse(age_cat == 1, 1, 0),
	ac2 = ifelse(age_cat == 2, 1, 0),
	ac3 = ifelse(age_cat == 3, 1, 0))

# Step 3: Summarize the number of infants, children, and adults per household
household_summary <- household_data1 %>%
group_by(hid) %>%
summarise(
	infants = sum(ac1, na.rm = TRUE),
	children = sum(ac2, na.rm = TRUE),
	adults = sum(ac3, na.rm = TRUE)
	)

# Step 4: Assign weights to age categories
household_data1 <- household_data1 %>%
mutate(weighted_age = case_when(
	age_cat == 1 ~ 0.4,   # Infants
	age_cat == 2 ~ 0.5,   # Children
	age_cat == 3 ~ 1,     # Adults
	TRUE ~ NA_real_
	))

# Step 5: Calculate the household consumption equivalent scale (CES)
household_ces <- household_data1 %>%
group_by(hid) %>%
summarise(ces = sum(weighted_age, na.rm = TRUE))

# Step 6: Clean up temporary variables (optional)
household_data1 <- household_data1 %>%
select(-age_cat, -ac1, -ac2, -ac3, -weighted_age)

# Combine the summary data into the main dataset
household_data1 <- household_data1 %>%
left_join(household_summary, by = "hid") %>%
left_join(household_ces, by = "hid")

# Correct way to use rm() with a list of object names
rm(list = c("data_summary", "household_ces", "household_data", "household_summary"))

# (2.0.2) HOUSEHOLD CONSUMPTION DATA -------
summary_data <- read_dta("replication_data/sumaria_2001iv.dta")

# (2.0.2.1) Cleaning summary_data ----- 
summary_data <- summary_data%>%
select(conglome, vivienda, hogar, ubigeo, factorto, gashog2d, ingtexhd, ingtrahd,
	gru13hd1, gru13hd2, gru13hd3, gru23hd1, gru23hd2, gru33hd1, gru33hd2,
	gru43hd1, gru43hd2, gru53hd1, gru53hd2, gru63hd1, gru63hd2, gru73hd1,
	gru73hd2, gru83hd1, gru83hd2, defesp) 

# (2.0.2.2) Definitions of aggreagate household consumption and transfers: --------

# Aggregate consumption:
	# GASHOG2D = G05HD + IG06HD + G07HD + IG08HD + SG23 + SIG24 + GRU11HD + GRU12HD1 + GRU12HD2 + GRU13HD1 + GRU13HD2 + GRU13HD3 + GRU21HD + GRU22HD1 + GRU22HD2 + GRU23HD1 + GRU23HD2 + GRU23HD3 + GRU24HD + GRU31HD + GRU32HD1 + GRU32HD2 + GRU33HD1 + GRU33HD2 + GRU33HD3 + GRU34HD + GRU41HD + GRU42HD1 + GRU42HD2 + GRU43HD1 + GRU43HD2 + GRU43HD3 + GRU44HD + GRU51HD + GRU52HD1 + GRU52HD2 + GRU53HD1 + GRU53HD2 + GRU53HD3 + GRU54HD + GRU61HD + GRU62HD1 + GRU62HD2 + GRU63HD1 + GRU63HD2 + GRU63HD3 + GRU64HD + GRU71HD + GRU72HD1 + GRU72HD2 + GRU73HD1 + GRU73HD2 + GRU73HD3 + GRU74HD + GRU81HD + GRU82HD1 + GRU82HD2 + GRU83HD1 + GRU83HD2 + GRU83HD3 + GRU84HD

# Transfers to be subtracted: 
	# INGTEXHD - Income from current transfers from abroad
	# INGTRAHD - Income from current domestic money transfers
	# GRU13HD1 - Food - Public Donation
	# GRU13HD2 - Food - Private Donation
	# GRU13HD3 - food donated, regelated, etc.
	# GRU23HD1 - Clothing and footwear, Public Donation
	# GRU23HD2 - Clothing and Footwear, Private Donation
	# GRU33HD1 - Housing Rent, Fuel, Electricity, and Housing Maintenance - Public Donation
	# Housing - Public Donation
	# GRU33HD2 - Housing Rent, Fuel, Electricity, and Housing Maintenance - Private Donation
	# GRU43HD1 - Furniture and Furnishings - Private Donation
	# GRU43HD1 - Furniture and Fixtures, and Housing Maintenance - Public Donation
	# Public Donation
	# GRU43HD2 - Furniture and Fixtures, and Housing Maintenance - Private Donation
	# Private
	# GRU53HD1 - Health Care, Preservation and Medical Services - Public Donation
	# Public Donation
	# GRU53HD2 - Health Care, Preservation and Medical Services - Private Donation
	# Private Donation
	# GRU63HD1 - Transportation and Communications - Public Donation
	# GRU63HD2 - Transportation and Communications - Private Donation
	# GRU73HD1 - Recreation, Amusement, Cultural and Educational Services - Public Donation
	# Public Donation
	# GRU73HD2 - Recreation, Amusement, Cultural and Educational Services - Private Donation
	# Donated Private
	# GRU83HD1 - Other Goods and Services - Public Donation
	# GRU83HD2 - Other Goods & Services - Private Donation
	# gashog2d total quarterly expenditure
	# defesp spatial price deflator based on metropolitan lima

# Recoding missing variables
summary_data <- summary_data %>%
mutate(
	ingtexhd = replace_na(ingtexhd, 0), 
	ingtrahd = replace_na(ingtrahd, 0),
	gru13hd1 = replace_na(gru13hd1, 0),
	gru13hd2 = replace_na(gru13hd2, 0),
	gru13hd3 = replace_na(gru13hd3, 0),
	gru23hd1 = replace_na(gru23hd1, 0),
	gru23hd2 = replace_na(gru23hd2, 0),
	gru33hd1 = replace_na(gru33hd1, 0),
	gru33hd2 = replace_na(gru33hd2, 0),
	gru43hd1 = replace_na(gru43hd1, 0),
	gru43hd2 = replace_na(gru43hd2, 0),
	gru53hd1 = replace_na(gru53hd1, 0),
	gru53hd2 = replace_na(gru53hd2, 0),
	gru63hd1 = replace_na(gru63hd1, 0),
	gru63hd2 = replace_na(gru63hd2, 0),
	gru73hd1 = replace_na(gru73hd1, 0),
	gru73hd2 = replace_na(gru73hd2, 0),
	gru83hd1 = replace_na(gru83hd1, 0),
	gru83hd2 = replace_na(gru83hd2, 0)
	)

# Removing departments not in sample, like in household data
summary_data <- summary_data %>%
mutate(
  ccdd = substr(ubigeo, 1, 2), # ccdd gets the first 2 characters of ubigeo (representing the department).
	ccpp = substr(ubigeo, 3, 4), # ccpp gets the next 2 characters of ubigeo (representing the province).
	ccdi = substr(ubigeo, 5, 6), # ccdi gets the final 2 characters of ubigeo (representing the district).
	depprov = substr(ubigeo, 1, 4),
	ccdd = as.numeric(ccdd), # Convert these variables to numeric
	ccpp = as.numeric(ccpp),
	ccdi = as.numeric(ccdi),
	depprov = as.numeric(depprov)
	) %>%
filter(ccdd %in% c(3, 4, 5, 8, 21)) # Filter the data to keep only rows where ccdd is 3, 4, 5, 8, or 21 (departments in sample)

data_merged <- household_data1 %>% left_join(summary_data)

data_merged <- data_merged %>% mutate(hconsump = gashog2d - ingtexhd - ingtrahd - gru13hd1 - gru13hd2 - gru13hd3 - 
	gru23hd1 - gru23hd2 - gru33hd1 - gru33hd2 - gru43hd1 - gru43hd2 - 
	gru53hd1 - gru53hd2 - gru63hd1 - gru63hd2 - gru73hd1 - gru73hd2 - 
	gru83hd1 - gru83hd2) %>% # calculate household consumption minus transfers 
select(-starts_with("gru"), -factorto, -ingtrahd, -ingtexhd, -gashog2d)  %>% # remove transfers 
mutate(hconsumplm = hconsump/defesp, # Household consumption minus transfers in Lima prices
	hhequiv= hconsumplm/ces, # Calculating household equivalent consumption and its log 
	lhhequiv=log(hhequiv),
	lhhequiv = replace_na(lhhequiv, 0),
	lhhconsplm=log(hconsumplm),
	lhhconsplm = replace_na(lhhconsplm, 0)) %>% filter(hhead == 1) # have 1 unique value for each household

# (2.0.3) Language and Ethincity data -------
language_data <- read_dta("replication_data/enaho01b_2001iv.dta")

language_data <- language_data %>% 
select(conglome, vivienda, hogar, ubigeo, q21, q24, q25, q231, q232, q233, q28, q31, q301, q302, q303) %>% 
mutate(
	ccdd = substr(ubigeo, 1, 2),
	ccpp = substr(ubigeo, 3, 4),
	ccdi = substr(ubigeo, 5, 6),
	depprov = substr(ubigeo, 1, 4),
	ccdd = as.numeric(ccdd), # Convert these variables to numeric
	ccpp = as.numeric(ccpp),
	ccdi = as.numeric(ccdi),
	depprov = as.numeric(depprov)
	) %>%
filter(ccdd %in% c(3, 4, 5, 8, 21)) %>%  # Filter the data to keep only rows where ccdd is 3, 4, 5, 8, or 21 (departments in sample)
rename(CAST = q231, QUE = q232, AYM = q233) %>% # All code within this mutate relates to coding which langauge is spoken in the household
mutate(CAST = if_else(q21 == 1, 1, CAST),
	CAST = if_else(q24 != 1 & !is.na(q24), 0, CAST),
	QUE = case_when(
		QUE == 2 ~ 1,    # Recode QUE 2 to 1
		TRUE ~ QUE       # Keep original values for other cases
		),
	QUE = if_else(q21 == 2, 1, QUE),  # Replace QUE with 1 if q21 == 2
	QUE = if_else(!is.na(q24) & q24 != 2, 0, QUE),
	AYM = case_when(
		AYM == 3 ~ 1,    # Recode AYM 3 to 1
		TRUE ~ AYM       # Keep original values for other cases
		),
	AYM = if_else(q21 == 3, 1, AYM),  # Replace AYM with 1 if q21 == 3
	AYM = if_else(!is.na(q24) & q24 != 3, 0, AYM),
	q301 = case_when(
		q28 == 1 ~ 1,
		!is.na(q31) & q31 != 1 ~ 0,
		TRUE ~ q301
		),
	q302 = case_when(
		q302 == 2 ~ 1,
		q28 == 2 ~ 1,
		!is.na(q31) & q31 != 2 ~ 0,
		TRUE ~ q302
		),
	q303 = case_when(
		q303 == 3 ~ 1,
		q28 == 3 ~ 1,
		!is.na(q31) & q31 != 3 ~ 0,
		TRUE ~ q303
		),
	CAST = if_else(is.na(CAST), q301, CAST),
	QUE = if_else(is.na(QUE), q302, QUE),
	AYM = if_else(is.na(AYM), q303, AYM),
	CAST = replace_na(CAST, 0),
	QUE = replace_na(QUE, 0),
	AYM = replace_na(AYM, 0))

# Drop all columns starting with 'q'
language_data <- select(language_data, -starts_with("q", ignore.case = FALSE))

# (2.0.4) Using datasets created so far ---------
consumption <- full_join(language_data, data_merged)
# TODO: Translate all labels to English if time)
consumption <- consumption %>%  mutate(ubigeo = as.numeric(ubigeo))

write_csv(consumption, "output/consumption.csv") 
saveRDS(consumption, "output/consumption.rds")

rm(list = c("data_merged", "household_data1", "language_data", "summary_data"))

# (1) Regressions --------
consumption <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/consumption.rds")
# OR 
# consumption <- read_csv("output/consumption.csv")
gis_dist <- read_dta("replication_data/gis_dist.dta")


# Keeping the observations from gis_dist (study region) for which we have consumption data
data1 <- left_join(gis_dist, consumption, by = "ubigeo") %>% filter(!is.na(hhead))
nrow(data1) # 1638 
summary(data1)

# (2.1) Household consumption regressions -------------
# (2.1.1) Cubic polynomial in latitude and longitude ------

# (A) Filter the data for distance < 100 km and excluding cusco from the analysis
reg1a_data <- subset(data1, cusco != 1 & d_bnd < 100)

# Perform the regression
model1a <- lm(lhhequiv ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1a_data)

model1a

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se1a <- vcovCL(model1a, cluster = ~ ubigeo)
summary_1a <- summary(model1a, robust = cluster_se1a) # The standard error estimates are different, probably because of another method being the standard in STATA. 
summary_1a
# (B) Filter the data for distance < 75 km and excluding Cusco from the analysis
reg1b_data <- subset(data1, cusco != 1 & d_bnd < 75)

# Perform the regression
model1b <- lm(lhhequiv ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1b_data)

model1b

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se1b <- vcovCL(model1b, cluster = ~ ubigeo)
summary_1b <- summary(model1b, robust = cluster_se1b)
summary_1b

# (C) Filter the data for distance < 50 km and excluding Cusco from the analysis
reg1c_data <- subset(data1, cusco != 1 & d_bnd < 50)

# Perform the regression
model1c <- lm(lhhequiv ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1c_data)

model1c

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se1c <- vcovCL(model1c, cluster = ~ ubigeo)
summary_1c <- summary(model1c, robust = cluster_se1c)
summary_1c

# (2.1.2) Cubic polynomial in distance to Potosi ------

# (A) Distance < 100 km
# Perform the regression, reusing the data

model2a <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2 + dpot3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1a_data)

model2a

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se2a <- vcovCL(model2a, cluster = ~ ubigeo)
summary_2a <- summary(model2a, robust = cluster_se2a)
summary_2a

# (B) Distance < 75 km
# Perform the regression, reusing the data

model2b <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2 + dpot3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1b_data)

model2b

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se2b <- vcovCL(model2b, cluster = ~ ubigeo)
summary_2b <- summary(model2b, robust = cluster_se2b)
summary2b

# (C) Distance < 50 km 
# Perform the regression, reusing the data

model2c <- lm(lhhequiv ~ pothuan_mita + dpot + dpot2 + dpot3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1c_data)

model2c

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se2c <- vcovCL(model2c, cluster = ~ ubigeo)
summary_2c <- summary(model2c, robust = cluster_se2c)
summary_2c

# (2.1.3) Cubic polynomial in distance to mita boundary ------

# (A) Distance < 100 km
# Perform the regression, reusing the data

model3a <- lm(lhhequiv ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1a_data)

model3a

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se3a <- vcovCL(model3a, cluster = ~ ubigeo)
summary_3a <- summary(model3a, robust = cluster_se3a)
summary_3a

# (B) Distance < 75 km
# Perform the regression, reusing the data

model3b <- lm(lhhequiv ~ pothuan_mita +dbnd_sh + dbnd_sh2 + dbnd_sh3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1b_data)

model3b

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se3b <- vcovCL(model3b, cluster = ~ ubigeo)
summary_3b <- summary(model3b, robust = cluster_se3b)
summary_3b

# (C) Distance < 50 km 
# Perform the regression, reusing the data

model3c <- lm(lhhequiv ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1c_data)

model3c

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se3c <- vcovCL(model3c, cluster = ~ ubigeo)
summary_3c <- summary(model3c, robust = cluster_se3c)
summary_3c

# (2.3) CHILDHOOD STUNTING ------------
ctalla <- read_dta("replication_data/ctalla.dta")

ctalla <- ctalla %>% filter(situacion == "") %>% 
  mutate(desnu = if_else(cnive %in% c("3", "4", "5"), 1, 0),
         desnu_sev = if_else(cnive %in% c("4", "5"), 1, 0))

ctalla <- ctalla %>% 
  mutate(canios = as.numeric(canios),
         cmeses = as.numeric(cmeses),
         canios = canios * 12,
         age_mo = canios + cmeses,
         age_mo = age_mo/3, # this transformation is not clear to me. 
         age_mo = floor(age_mo),
         sexo = as.numeric(sexo),
         sexo = if_else(sexo == 2, 0, sexo)) %>% 
  rename(male = sexo)

# Creating factor columns for age_mo and renaming them
ctalla1 <- cbind(ctalla, model.matrix(~factor(ctalla$age_mo) - 1))
ctalla1 <- ctalla1 %>%
  rename_with(~ paste0("yr", gsub("factor\\(ctalla\\$age_mo\\)", "", .x)), 
              matches("^factor\\(ctalla\\$age_mo\\)[0-9]+$"))

# creating pariwise age - sex interactions
ctalla1 <- ctalla1 %>% mutate(across(starts_with("yr"), 
                .fns = ~ .x * male, 
                .names = "{.col}_sex"))

ctalla1 <- ctalla1 %>% select(cod_mod, codgeo, z_score, talla_cm, male, desnu, desnu_sev,
                              starts_with("yr"))

# Renaming columns to match exactly with stata code
ctalla1 <- ctalla1 %>% rename_with(~ sub("^yr(\\d+)_sex$", "age_sex\\1", .x), 
              matches("^yr\\d+_sex$"))

ctalla1 <- ctalla1 %>% rename(ubigeo = codgeo) %>% mutate(ubigeo = as.numeric(ubigeo))

# Saving output for use in other tables
write_csv(ctalla1, "output/height.csv") 
saveRDS(ctalla1, "output/height.rds")

# Height Regressions -------
gis_dist <- read_dta("replication_data/gis_dist.dta")
height <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/height.rds")

# Keeping the observations from gis_dist (study region) for which we have height data
data2 <- left_join(gis_dist, height, by = "ubigeo") 
nrow(data2) # 185661 obs of 86 variables 
summary(data2)

# (2.3.1) Cubic poolynomial longitude and latitude ------

# (A) Distance < 100 
reg1a_height_data <- subset(data2, cusco != 1 & d_bnd < 100)

# Perform the regression
model1a_height <- lm(desnu ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1a_height_data)

model1a_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se1a_height <- vcovCL(model1a_height, cluster = ~ ubigeo)
summary_1a_height <- summary(model1a_height, robust = cluster_se1a_height)  
summary_1a_height

# (B) Distance < 75
reg1b_height_data <- subset(data2, cusco != 1 & d_bnd < 75)

# Perform the regression
model1b_height <- lm(desnu ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1b_height_data)

model1b_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se1b_height <- vcovCL(model1b_height, cluster = ~ ubigeo)
summary_1b_height <- summary(model1b_height, robust = cluster_se1b_height) # Strongly significant vs somewhat significant in paper
summary_1b_height

# (C) Distance < 50
reg1c_height_data <- subset(data2, cusco != 1 & d_bnd < 50)

# Perform the regression
model1c_height <- lm(desnu ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1c_height_data)

model1c_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se1c_height <- vcovCL(model1c_height, cluster = ~ ubigeo)
summary_1c_height <- summary(model1c_height, robust = cluster_se1c_height)
summary_1c_height
# (D) Border District
reg1d_height_data <- subset(data2, cusco != 1 & border ==1)

# Perform the regression
model1d_height <- lm(desnu ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3 , data = reg1d_height_data)

model1d_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se1d_height <- vcovCL(model1d_height, cluster = ~ ubigeo)
summary_1d_height <- summary(model1d_height, robust = cluster_se1d_height)
summary_1d_height

# (2.3.2) Cubic poolynomial in distance to Potosi ------

# Reusing the data
# (A) Distance < 100 
model2a_height <- lm(desnu ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1a_height_data)

model2a_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se2a_height <- vcovCL(model2a_height, cluster = ~ ubigeo)
summary_2a_height <- summary(model2a_height, robust = cluster_se2a_height)
summary_2a_height

# (B) Distance < 75
model2b_height <- lm(desnu ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1b_height_data)

model2b_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se2b_height <- vcovCL(model2b_height, cluster = ~ ubigeo)
summary_2b_height <- summary(model2b_height, robust = cluster_se2b_height)
summary_2b_height

# (C) Distance < 50
model2c_height <- lm(desnu ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1c_height_data)

model2c_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se2c_height <- vcovCL(model2c_height, cluster = ~ ubigeo)
summary_2c_height <- summary(model2c_height, robust = cluster_se2c_height)
summary_2c_height

# (D) Border District
model2d_height <- lm(desnu ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1d_height_data)

model2d_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se2d_height <- vcovCL(model2d_height, cluster = ~ ubigeo)
summary_2d_height <- summary(model2d_height, robust = cluster_se2d_height)
summary_2d_height

# (2.3.3) Cubic poolynomial in distance to mita boundary ------
# Resuing the data, as before

# (A) Distance < 100
model3a_height <- lm(desnu ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1a_height_data)

model3a_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se3a_height <- vcovCL(model3a_height, cluster = ~ ubigeo)
summary_3a_height <- summary(model3a_height, robust = cluster_se3a_height)
summary_3a_height

# (B) Distance < 75
model3b_height <- lm(desnu ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1b_height_data)

model3b_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se3b_height <- vcovCL(model3b_height, cluster = ~ ubigeo)
summary_3b_height <- summary(model3b_height, robust = cluster_se3b_height)
summary_3b_height

# (C) Distance < 50
model3c_height <- lm(desnu ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1c_height_data)

model3c_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se3c_height <- vcovCL(model3c_height, cluster = ~ ubigeo)
summary_3c_height <- summary(model3c_height, robust = cluster_se3c_height)
summary_3c_height

# (D) Border District
model3d_height <- lm(desnu ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2+ bfe4_3, data = reg1d_height_data)

model3d_height

# Calculate robust standard errors clustered by 'ubigeo'
cluster_se3d_height <- vcovCL(model3d_height, cluster = ~ ubigeo)
summary_3d_height <- summary(model3d_height, robust = cluster_se3d_height)
summary_3d_height

# PRODUCING FINAL REPLICATION TABLE ----------

# Have to first extract all the information we want #####
# List of model names and corresponding datasets
model_names <- c("model1a", "model1a_height", "model1b", "model1b_height", "model1c", 
                 "model1c_height", "model1d_height", "model2a", "model2a_height", "model2b", 
                 "model2b_height", "model2c", "model2c_height", "model2d_height", 
                 "model3a", "model3a_height", "model3b", "model3b_height", "model3c", 
                 "model3c_height", "model3d_height")

# List of corresponding dataset names
dataset_names <- c("reg1a_data", "reg1a_height_data", "reg1b_data", "reg1b_height_data", "reg1c_data", 
                   "reg1c_height_data", "reg1d_height_data", "reg1a_data", "reg1a_height_data", "reg1b_data", "reg1b_height_data", "reg1c_data", 
                   "reg1c_height_data", "reg1d_height_data", "reg1a_data", "reg1a_height_data", "reg1b_data", "reg1b_height_data", "reg1c_data", 
                   "reg1c_height_data", "reg1d_height_data")

# Initialize an empty list to store results
results_list <- list()

# Function to extract information from a model summary
extract_model_info <- function(model_name, dataset_name) { # model_name is an input like "1a" or "1a_height"
  # Get the model summary object
  model_summary <- get(paste0("summary_", sub("model", "", model_name)))
  
  # Get the dataset
  dataset <- get(dataset_name)
  
  # Extract relevant information
  coef_pothuan_mita <- model_summary$coefficients["pothuan_mita", "Estimate"]
  se_pothuan_mita <- model_summary$coefficients["pothuan_mita", "Std. Error"]
  r_squared <- model_summary$r.squared
  n_obs <- length(model_summary$residuals)
  
  # Assuming 'ubigeo' is the cluster variable, extract number of clusters
  n_clusters <- length(unique(dataset$ubigeo))
  
  # Return as a data frame
  return(data.frame(
    Model = model_name,
    Coefficient = coef_pothuan_mita,
    Std_Error = se_pothuan_mita,
    R_Squared = r_squared,
    N_Obs = n_obs,
    N_Clusters = n_clusters
  ))
}

# Loop over the models and extract information
for (i in seq_along(model_names)) {
  model_name <- model_names[i]
  dataset_name <- dataset_names[i]
  
  model_info <- extract_model_info(model_name, dataset_name)
  results_list[[model_name]] <- model_info
}

# Combine all results into a single data frame
results_table <- do.call(rbind, results_list)
saveRDS(results_table, "output/Table2_replication_results.rds")

# View the summary table
print(results_table)





