# NOTE: These were originally separate files that I have combined to create a coding sample

# Replication Project for Causal Inference Exam # 

# Goal: Replicate and Extend "The Persisent Effects of Peru's Mining Mita" by Melissa Dell, published in Econometrica in 2010. 

# Author: Lolakshi Rajlakshmi (12339702)

# This project consists of 3 files, to be run in the following order
# 1. repfile_1.r: Create geographic variables 
# 2. Table2.R: Replicating Table 2 - Main Results
# 3. Table5.R: Replicating Table 5 - 1572 tribute and Population
# 4. Table1.R: Replicating Table 1 - Summary Statistics
# 5. Extension_rdrobust.R - Validating results with newer, refined package
# 6. Falsificationtest.R - Falsification tests a la Keele & Titiunik 2015 suggestions

# Dell(2010) has a replication package available at https://scholar.harvard.edu/dell/publications/persistent-effects-perus-mining-mita . This package contains 
# (i) Stata files for replicating the tables from the paper,
# (ii) R, Python and ArcGIS files for replicating the figures, and 
# (iii) the required data.   


# (0) Preliminaries -----------------------------------

# Clear all
rm(list = ls())

# Disabling scientific notation - optional
options(scipen = 999)

# Note on commenting and word wrap: I have word wrap enabled and so have not created separate lines for comments/commands that will be too long to read in one line. If possible, please enable this in your IDE/ text editor to make going through this much easier. 

# Loading required packages
dependencies <- c("tidyverse", "lmtest", "sandwich", "readr", "haven" , "data.table", "labelled") # TODO: Define required packages
install.packages(dependencies) # installs all packages mentioned in dependencies, choose Yes if prompted "to install from sources the  packages which need compilation".
lapply(dependencies, library, character.only = TRUE) # loads all packages from dependencies into current working environment

# Package descriptions: 
# tidyverse: data manipulation, wrangling and tidying
# lmtest and sandwich: regressions and standard
# readr: quickly read csv files
# haven: import and create .dta files (STATA standard)
# labelled: Add metadata (variable, value labels and SPSS-style missing values) to vectors.

# Set working directory
setwd("/Users/BRAJENDRA2/Courses/CausalInference/Term Paper/Submission/") # Please insert your individual path here, if needed

# (0.1) Creating Geospatial variables -----------------------------------
# The author created these input files with GIS software. Since I do not have access to them, I use the files directly as provided in the replication package. 

# The output from this section is required for creating all the other tables. There are two files we want, gis_dist.dta/gis_dist.rda and gis_grid.dta/gis_grid.rds # TODO: Compare with already existing gis_dist and gis_grid.dta files by exporting dta using haven and show results

# The output creates geographic controls using district - level data and grid cell - level data
# I created csv files with MS Excel for several files that were originally .dbf files because I was unable to directly use them. Both dbf and csv files are included in the submission. 

# Loading the data 
d2_huand <- read_csv("replication_data/d2_huand.csv") # TODO: Add descriptions of these dataframes if you can - as in what they do/ where they came from. 
d2_potd <- read_csv("replication_data/d2_potd.csv")
dbnd <- read_csv("replication_data/dbnd.csv")
dis_xy <- read_dta("replication_data/dis_xy.dta")
e3 <- read_csv("replication_data/e3.csv")
sldist <- read_csv("replication_data/sldist.csv")
elevdis <- read_csv("replication_data/elevdis.csv")
inmita <- read_csv("replication_data/inmita.csv")
outmita <- read_csv("replication_data/outmita.csv")
gr_coor <- read_csv("replication_data/gr_coor.csv") 
s3 <- read_csv("replication_data/s3.csv")

#Run gis.do to create the Stata files gis dist.dta and gis grid.dta, which contain the geographic variables required for reproducing Tables 1 through 9 
# FIXME: remove above comment

# (0.1.1) Control Type 1: Creating Geographic Controls by District -----------------------------------
## 1. Distance to Mita boundary - This gives distance from district capitals to the nearest point on the mita boundary, as well as the geographic location of that point

# Create distance variable
dbnd <- dbnd %>% # this dataset contains variables specifying the distance to the boundary 
  mutate(d_bnd = near_dist / 1000) %>% # This is the distance to the mita boundary in km
  mutate(temp = ifelse(near_y <= 8400000, 1, 0)) # TODO: what does this do?

# Creating variables bfe4_1, bfe4_2, bfe4_3, the boundary fixed effects which specify which boundary segment the district capital is closest to
dbnd <- dbnd %>%
  mutate(bfe4_1 = ifelse(temp == 1 & near_x <= 824796.228990515, 1, 0),
         bfe4_2 = ifelse(temp == 1 & bfe4_1 != 1, 1, 0),
         bfe4_3 = ifelse(temp == 0 & near_x <= 808697.919841043, 1, 0))

# Keep necessary columns and save as gis_dist
gis_dist <- dbnd %>%
  select(d_bnd, bfe4_1, bfe4_2, bfe4_3, ubigeo, pothuan_mita, border, cusco, near_x, near_y) %>%
  arrange(ubigeo) # TODO: Why these variables and what are they?
# ubigeo is unique district identifier, d_bnd the district to boundary, bfe variables are the boundary fixed effects, pothuan_mita and cusco are variables about , border, near_x and near_y are 

# TODO: Look at variables tagged as destring variables from stata code and confirm whether they have been loaded as numeric values in R. use class or summary or str() and then mutate as.numeric
saveRDS(gis_dist, "gis_dist.rds") # Save output

## 2. Mean Elevation of the District - The mean area weighted elevation of each district
# Required file - import of elevdis.dbf/ elevdis.csv

# Rename and process
elevdis <- elevdis %>%
  rename(ubigeo = CODIGO_DIS, elv_sh = MEAN) %>% # TODO: destring ubigeo - what does this mean? The destring command in Stata is used to convert string variables that contain numbers into numeric variables. It translates numeric values stored as strings into numeric values that Stata can recognize. This allows for numerical analysis of the data.
  mutate(elv_sh = elv_sh / 1000) # elv_sh is the elevation in 1000m 

# Merge with gis_dist and save
gis_dist <- readRDS("gis_dist.rds") %>%
  left_join(elevdis, by = "ubigeo") %>%
  filter(!is.na(elv_sh))  # Keep only districts within 100km of mita boundary 
  # TODO: Why does !isna(elv_sh) ensure districts within 100km? Stata code: keep if _merge==3 /*only keep districts within 100 km of mita boundary*/ 
  # Because: The command "keep if _merge == 3" in Stata is used to keep only the matched observations after merging two datasets. This means that it retains only the observations that have matching values in both datasets based on the merge key.
  # So which variable in gis_dist has 100 km variable?

saveRDS(gis_dist, "gis_dist.rds") # save updated output

## 3. Mean Slope of the District
# Required file - import of sldist.dbf/ sldist.csv

# Rename 
sldist <- sldist %>%
  rename(ubigeo = CODIGO_DIS, slope = MEAN)

# Merge with gis_dist and save
gis_dist <- readRDS("gis_dist.rds") %>%
  left_join(sldist, by = "ubigeo") %>%
  filter(!is.na(slope))  # Keep only districts within 100km of mita boundary

saveRDS(gis_dist, "gis_dist.rds")

## 4. Distance to Potosi
# Required file - import of d2_potd.csv

# Process
d2_potd <- d2_potd %>%
  mutate(dpot = near_dist / 1000) # dpot is distance to Potosi(km)

# Merge with gis_dist and save
gis_dist <- readRDS("gis_dist.rds") %>%
  left_join(d2_potd, by = "ubigeo") %>%
  filter(!is.na(dpot))  # Keep only districts within 100km of mita boundary

saveRDS(gis_dist, "gis_dist.rds")

## 4. Distance to Huancavelica
# Required file - import of d2_huand.csv

# Process
d2_huand <- d2_huand %>%
  mutate(dhuan = near_dist / 1000) # dhuan is distance to Huancavelica(km)

# Merge with gis_dist and save
gis_dist <- readRDS("gis_dist.rds") %>%
  left_join(d2_huand, by = "ubigeo") %>%
  filter(!is.na(dhuan))  # Only keep districts within 100km of mita boundary

saveRDS(gis_dist, "gis_dist.rds")

## 5. Latitude and Longitude
# Required file - import of dis_xy.dta

# Merge with gis_dist and save
gis_dist <- readRDS("gis_dist.rds") %>%
  left_join(dis_xy, by = "ubigeo") %>%
  filter(!is.na(lat) & !is.na(lon)) 

saveRDS(gis_dist, "gis_dist.rds")

## 6. Generate RD Terms

# Generate polynomial terms
gis_dist <- readRDS("gis_dist.rds") %>%
  mutate(elv_sh2 = elv_sh^2, elv_sh3 = elv_sh^3, elv_sh4 = elv_sh^4, # Polynomial terms for elevation
         slope2 = slope^2, slope3 = slope^3, slope4 = slope^4, # Polynomial terms for slope
         dbnd_sh = d_bnd / 100, dbnd_sh2 = dbnd_sh^2, dbnd_sh3 = dbnd_sh^3, dbnd_sh4 = dbnd_sh^4, # Polynomial terms for distance
         dpot2 = dpot^2, dpot3 = dpot^3, dpot4 = dpot^4, # Polynomial terms for distance to Potosi
         dhuan2 = dhuan^2, dhuan3 = dhuan^3, dhuan4 = dhuan^4, # Polynomial terms for distance to Huancavelica
         mita_neg = ifelse(pothuan_mita == 0, -1, pothuan_mita), # Create distance to boundary relative to cutoff point
         bnd_dist_neg = d_bnd * mita_neg) %>%
  select(-mita_neg)

# Generate latitude and longitude polynomial terms
gis_dist <- gis_dist %>%
  mutate(x = lon, y = lat) %>%
  group_by() %>% # group by district? 
  mutate(xbar = mean(x), ybar = mean(y)) %>%
  ungroup() %>%
  mutate(x = x - xbar, y = y - ybar) %>% # linear RD terms
  mutate(x2 = x^2, y2 = y^2, xy = x * y, # quadratic RD terms
         x3 = x^3, y3 = y^3, x2y = x^2 * y, xy2 = x * y^2, # cubic RD terms
         x4 = x^4, y4 = y^4, x3y = x^3 * y, x2y2 = x^2 * y^2, xy3 = x * y^3) # quartic RD terms

saveRDS(gis_dist, "gis_dist.rds") # FIXME: Need to replace older versions with successive save

# (0.1.2) Control Type 2: Creating Geographic Controls by Grid Cell  ----------------------------------
# These are used for the means comparision tests in Table 1

## 1. Distance to Mita boundary - gives distance from centroids to the nearest point on the mita boundary, as well as the geographic location of that point. Note that the entire grid cell is considered to be in the study region if its centrod is in the study region

# Files - imports of inmita.dbf and outmita.dbf
# TODO: what is pothuan - Potosi Huancaveleica dummy - go deeper - it seems to be whether it is the east or west side of the line dividing these areas. 
 # NOTE: This discussion suggests that exempt districts were those located relatively far from both Potosí and Huancavelica. The correlation between distance to Potosí and distance to Huancavelica is −0.996, making it impossible to separately identify the effect of distance to each mine on the probability of receiving treatment. Thus, I divide the sample into two groups—municipalities to the east and those to the west of the dividing line between the Potosí and Huancavelica mita catchment areas. When considering districts to the west (Potosí side) of the dividing line, a flexible specification of mita treatment on a cubic in distance to Potosí, a cubic in elevation, and their linear interaction shows that being 100 additional kilometers from Potosí lowers the probability of treatment by 0.873, with a standard error of 0.244. Being 100 meters higher increases the probability of treatment by 0.061, with a standard error of 0.027. When looking at districts to the east (Huancavelica side) of the dividing line and using an analogous specification with a polynomial in distance to Huancavelica, the marginal effect of distance to Huancavelica is negative but not statistically significant.

# Process
inmita <- inmita %>%
  mutate(d_bnd = near_dist / 1000, pothuan_mita = 1) %>% # d_bnd is distance to  mita boundary (km)
  filter(near_fid != -1)  # these are grid cells falling partially within the study region but whose centroids are > 100 km from the study boundary. Thus they should be dropped

outmita <- outmita %>%
  mutate(d_bnd = near_dist / 1000, pothuan_mita = 0) %>%
  filter(near_fid != -1)  # Remove grid cells > 100 km from study boundary

# Combine inmita and outmita
gis_grid <- inmita %>%
  bind_rows(outmita)

saveRDS(gis_grid, "gis_grid.rds")

## 1. Grid Cell Coordinates
# Files - import of gr_coor.dbf

# Process and merge with gis_grid
gr_coor <- gr_coor %>% rename(xcord = point_x, ycord = point_y) %>% arrange(GRID_ID)

gis_grid <- readRDS("gis_grid.rds") %>%
  left_join(gr_coor, by = "GRID_ID") %>%
  filter(!is.na(xcord) & !is.na(ycord))  # Keep only cells within 100km of study boundary

saveRDS(gis_grid, "gis_grid.rds")

## 2. Mean Elevation of the Grid Cell - includes Cusco, which we want to remove because it is historically important and more prosperous - "I exclude Cusco because part of its relative prosperity today likely relates to its pre-mita heritage as the Inca capital. When Cusco is included, the impacts of the mita are estimated to be even larger.''

# Files - import of e3.dbf

# Process
e3 <- e3 %>%
  rename(grid_id = VALUE) %>%
  mutate(elv_sh = MEAN / 1000) %>% # elevation in km
  select(grid_id, elv_sh)

# Merge with gis_grid and save
gis_grid <- readRDS("gis_grid.rds") %>%
  left_join(e3, by = "grid_id") %>%
  filter(!is.na(elv_sh))  # Keep only cells within 100km of study boundary

# keep if _merge==3  /*keep only the cells within 100 km of study boundary*/ the merge==2's are all outside the study region - there are 276 grid cells inside the study region*/ 
  # TODO: check!! that only variables that are matched are included in the merged dataset.
saveRDS(gis_grid, "gis_grid.rds")

## 3. Mean Slope of the Grid Cell
# Files - import of s3.dbf

# Process
s3 <- s3 %>%
  rename(grid_id = VALUE) %>%
  select(grid_id, slope = MEAN) # shayad upar hi rename karna hoga try karna
  # also have to replace slope = 0 if grid_id == 264 because this grid cell is primarliy Cusco - to NA karna hai ya 0?

# Merge with gis_grid and save
gis_grid <- readRDS("gis_grid.rds") %>%
  left_join(s3, by = "grid_id") %>%
  filter(!is.na(slope))  # Keep only cells within 100km of study boundary

saveRDS(gis_grid, "gis_grid.rds")

## 3. Generate RD Terms for Grid Cells

# Generate elevation polynomial terms
gis_grid <- readRDS("gis_grid.rds") %>%
  mutate(elv_sh2 = elv_sh^2, elv_sh3 = elv_sh^3, elv_sh4 = elv_sh^4,
         slope2 = slope^2, slope3 = slope^3, slope4 = slope^4,
         dbnd_sh = d_bnd / 100, dbnd_sh2 = dbnd_sh^2, dbnd_sh3 = dbnd_sh^3, dbnd_sh4 = dbnd_sh^4,
         mita_neg = ifelse(pothuan_mita == 0, -1, pothuan_mita),
         bnd_dist_neg = d_bnd * mita_neg) %>%
  select(-mita_neg)

# Generate latitude and longitude polynomial terms
gis_grid <- gis_grid %>%
  mutate(x = xcord, y = ycord) %>%
  group_by() %>%
  mutate(xbar = mean(x), ybar = mean(y)) %>%
  ungroup() %>%
  mutate(x = x - xbar, y = y - ybar) %>%
  mutate(x2 = x^2, y2 = y^2, xy = x * y,
         x3 = x^3, y3 = y^3, x2y = x^2 * y, xy2 = x * y^2,
         x4 = x^4, y4 = y^4, x3y = x^3 * y, x2y2 = x^2 * y^2, xy3 = x * y^3)

saveRDS(gis_grid, "gis_grid.rds")

# Export to DTA (if needed) To export your final data to Stata .dta format, you can use the haven package:

library(haven)

# For district-level data
write_dta(gis_dist, "gis_dist.dta")

# For grid cell-level data
write_dta(gis_grid, "gis_grid.dta")

rm(list = ls())

# ------------------------ Table2.R -------------------#

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

# -------- Table5.R ----------------------------------
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


# ------------------------- Table1.R ------------------------------- 
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

# ---------------------- Extension_rdrobust.R -------------------------------
install.packages("rdrobust")
library(rdrobust)
library(haven)
rm(list = ls())

# Extending Table II's results (living standards) with rdrobust -------

# Required data
height <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/height.rds")
consumption <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/consumption.rds")
gis_dist <- read_dta("replication_data/gis_dist.dta")

# (1) Consumption in 2001 #####
data1 <- left_join(gis_dist, consumption, by = "ubigeo") %>% filter(!is.na(hhead))

# Score/Running variable = distance to mita boundary, data1$d_bnd
# Cutoff = 0, as treatment switched exactly at the boundary
hhcons <- data1$lhhequiv
summary(hhcons)

data_try <- data1 %>% mutate(d_bnd = if_else(pothuan_mita == 0, -d_bnd, d_bnd )) # if household is in control group, code distance as negative. This is to obtain values on both sides of the cutoff as required in this command. 

distance <- data1$d_bnd
summary(distance)

distance_try <- data_try$d_bnd
summary(distance_try)

# Create an RD Plot showing variation in housing consumption
rdplot(y = hhcons, x = distance_try, binselect = "es", ci = 95, # binselect = "espr" yields same result
       title = "RD Plot: Household Consumption across Mita boundary",
       y.label = "Log of Household Consumption Equivalent",
       x.label = "Distance from Mita boundary(km)")

savePlot()

# REMARK 1—Discrete Running Variable: Assumption 1(a) rules out discrete-valued running variables. In applications where Xi exhibits many mass points near the cutoff, this assumption may still give a good approximation and our results might be used in practice. However, when Xi exhibits few mass points, our results do not apply directly without further assumptions and modifications, and other assumptions and inference approaches may be more appropriate; see, for example, Cattaneo, Frandsen, and Titiunik (2014). 

rd_result_1 <- rdrobust(y = hhcons, x = distance_try, all = TRUE)

rd_result_1_cov <- rdrobust(y = hhcons, x = distance_try, all = TRUE,
                            covs = cbind(data_try$infants, data_try$children, data_try$adults, data_try$elv_sh, data1$slope))

summary(rd_result_1)
summary(rd_result_1_cov)

# (2) Stunted growth in 2005 ----

# Keeping the observations from gis_dist (study region) for which we have height data
data2 <- left_join(gis_dist, height, by = "ubigeo") 

# Score/Running variable = distance to mita boundary, data1$d_bnd
# Cutoff = 0, as treatment switched exactly at the boundary
height_kid <- data2$talla_cm %>% as.numeric()
summary(height_kid)

data2_t <- data2 %>% mutate(d_bnd = if_else(pothuan_mita == 0, -d_bnd, d_bnd )) # if household is in control group, code distance as negative. This is to obtain values on both sides of the cutoff as required in this command. 

summary(data2$d_bnd)
summary(data2_t$d_bnd) # Check that we have distances on both sides. 
distance_height <- data2_t$d_bnd

# Create an RD Plot showing variation in housing consumption
rdplot(y = height_kid, x = distance_height, binselect = "es", ci = 95, # binselect = "espr" yields same result
       title = "RD Plot: Height of children across Mita boundary",
       y.label = "Height",
       x.label = "Distance from Mita boundary(km)")

rdplot(y = data2_t$desnu,  x = distance_height, binselect = "es", ci = 95, # binselect = "espr" yields same result
       title = "RD Plot: Stunted growth of children across Mita boundary",
       y.label = "Stunted Growth of Children",
       x.label = "Distance from Mita boundary(km)")

rd_result_2_height <- rdrobust(y = height_kid, x = distance_height, all = TRUE, 
                               covs = cbind(data2_t$elv_sh, data2_t$slope)) # Takes some time to run

rd_result_2_desnu <- rdrobust(y = data2_t$desnu, x = distance_height, all = TRUE,
                            covs = cbind(data2_t$elv_sh, data2_t$slope))

summary(rd_result_2_height)
summary(rd_result_2_desnu)

# ---------------------------- Falsificationtest.R --------------------------
library(foreign)
library(Matching)
library(haven)
library(tidyverse)
library(fields) # I had some trouble with this and had to reinstall it 1-2 times. Please make sure that you do this if it does not work - sorry! 

rm(list = ls())

# Falsification tests a la Keele & Titiunik 2015 suggestions

# (1) Creating spatial matches between Treatment and Control Groups ----------

# Required data for living standards balance tests
consumption <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/consumption.rds")
gis_dist <- read_dta("~/Courses/CausalInference/Term Paper/Submission/replication_data/gis_dist.dta")
data1 <- left_join(gis_dist, consumption, by = "ubigeo") %>% filter(!is.na(hhead))
# Sourcing some distance functions
source("~/Courses/CausalInference/Term Paper/Submission/distance-functions.R") # Defines functions such as disvec that we use below
set.seed(984566)

# Creating treatment and control vectors, geographical vectors required further
dim(data1)

# Select geographic variables, treatment indicator and an identifier
dat <- data1[,c("lat", "lon", "pothuan_mita", "hid")]
dat <- as.data.frame(dat)

# Match on chord distance
Ntr = sum(dat$pothuan_mita == 1) # Number of treatment units
Nco = sum(dat$pothuan_mita == 0) # Number of control units
N = length(dat$pothuan_mita) # Total number of observations

# The distance functions we use need all coordinates to be positive, take abs()
lat.tr = abs(dat[dat$pothuan_mita == 1, "lat"]) # Create a vector containing latitude values for treatment group

lat.co = abs(dat[dat$pothuan_mita == 0,"lat"]) # Create a vector containing latitude values for control group
lon.tr = abs(dat[dat$pothuan_mita == 1,"lon"])  # For longitude
lon.co = abs(dat[dat$pothuan_mita == 0,"lon"]) 
indx.tr = dat[dat$pothuan_mita == 1,"hid"] # Creating an indexing vector with unique identifier for treatment group

indx.co = dat[dat$pothuan_mita == 0,"hid"] # Creating an indexing vector with unique identifier for treatment group

indx.co.relative = 1:Nco # Creates another indexing vector, with values 1: N(control group units)

colnames <- c("hid_tr", "indx.tr", "indx.co", "indx.co.rel", "distance") # Column names for result matrix

results <- matrix(NA, nrow=Ntr, ncol = length(colnames), dimnames = list(NULL, colnames)) # Create empty matrix to store results

for(i in 1:Ntr) { # For all treatment units
  t0 = proc.time()[3] # measures time for some reason? 
  dist = as.vector(disvec(lat.tr[i], lon.tr[i], lat.co, lon.co)$chord) # applying disvec to obtain chord ie. chordal distance 
  mindist = min(dist)
  id.co = indx.co[dist==mindist]
  id.co.rel = indx.co.relative[dist==mindist]
  n = length(id.co)  
  if(n > 1) {
    k = sample(1:n, 1)
    id.co = id.co[k]   # break ties randomly
    id.co.rel = id.co.rel[k]
  }
  #results <- rbind(results, cbind(indx.tr = i, indx.co = id.co, distance = mindist))
  results[i,] <- c(i,indx.tr[i],id.co,id.co.rel,mindist)
  t1 = proc.time()[3]
  cat("Found match for treatment", i, "of", Ntr, " in ", t1-t0, "seconds -- Matched control is", id.co, "\n")
}

# Checking which control units are selected
unique(results[,3]) # 167 units are selected out of 526, which is a lot more than the replication package from Keele and Titiunik (2015) 

saveRDS(results, file = "output/chordmatch_fulldata.rds")

# (2) Balance test before matching -----------
rm(list = ls())

## Open original, (unmatched) dataset
consumption <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/consumption.rds")
gis_dist <- read_dta("~/Courses/CausalInference/Term Paper/Submission/replication_data/gis_dist.dta")
data1 <- left_join(gis_dist, consumption, by = "ubigeo") %>% filter(!is.na(hhead))
dim(data1)

# covariates we would like to have balance on 
B <- data1[,c("elv_sh", "slope", "infants", "children", "adults")] %>% as.data.frame()  # TODO: Should I include d_bnd, distance to boundary?

Nt = sum(data1$pothuan_mita == 1)
Nc = sum(data1$pothuan_mita == 0)

treat = data1$pothuan_mita

colnames = c("Var" , "MeanT", "MeanC", "Pval ttest", "Var ratio", "Pval KS", "QQ med diff")
resbal = matrix(NA, nrow=ncol(B)*2 + 1, ncol = 7, dimnames = list(NULL, colnames))

for(i in 1:ncol(B)) {

  #cat("Calculating balance tests for variable", i, "of", ncol(B), "\n")
  indx = (!is.na(B[,i]) & treat==1)
  xtr  = B[indx,i]
  indx = (!is.na(B[,i]) & treat==0)
  xco  = B[indx,i]
  
  bal<-balanceUV(xtr,xco, ks=TRUE,nboots = 1000, paired=FALSE, match=FALSE)

  sigmat<-sqrt(bal$var.Tr)      
  sigmac<-sqrt(bal$var.Co)
  #sigmaboth<-sqrt(var(B[,i])*(1/Nt+1/Nc))

  resbal[2*(i-1)+1,1] <- names(B)[i]
  resbal[2*(i-1)+1,2] <- round(bal$mean.Tr,digits=3)
  resbal[2*(i-1)+2,2] <- paste("(",round(sigmat/sqrt(Nt),digits=3),")",sep="")
  resbal[2*(i-1)+1,3] <- round(bal$mean.Co,digits=3)
  resbal[2*(i-1)+2,3] <- paste("(",round(sigmac/sqrt(Nc),digits=3),")",sep="")   # st err of mean.Co
  resbal[2*(i-1)+1,4] <- round(bal$p.value,digits=3)
  resbal[2*(i-1)+1,5] <- round(bal$var.ratio,digits=3)
  resbal[2*(i-1)+1,6] <- round(bal$ks$ks.boot.pvalue,digits=3)  # bootstrapped KS  pvalue
  resbal[2*(i-1)+1,7] <- round(bal$qqsummary$mediandiff,digits=3)  # bootstrapped KS  pvalue  
}
resbal[ncol(B)*2 + 1, 1] <- "Sample size (T,C)"
resbal[ncol(B)*2 + 1, 2] <- Nt
resbal[ncol(B)*2 + 1, 3] <- Nc

print(resbal)

saveRDS(resbal, file="output/prematch_balance_test.rds")

# (3) Balance test after matching ------------
rm(list = ls())
options(width=300)

## Load matched dataset
results <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/chordmatch_fulldata.rds")

## Open original, unmatched dataset
consumption <- readRDS("~/Courses/CausalInference/Term Paper/Submission/output/consumption.rds")
gis_dist <- read_dta("~/Courses/CausalInference/Term Paper/Submission/replication_data/gis_dist.dta")
data1 <- left_join(gis_dist, consumption, by = "ubigeo") %>% filter(!is.na(hhead))
dim(data1)

## keep full matched dataset
indx.tr <- results[,"indx.tr"] # hid of treatment unit, 1112 rows
indx.co <- results[,"indx.co"] # hid of matched control unit, also 1112 rows, but non-unique

# data.tr <- data1[indx.tr, ]
# Filter treatment group observations from master data
data.tr<- data1 %>% filter(hid %in% indx.tr) 
# Filter matched control group observations from master data
# data.co <- data1[indx.co, ]
order_df <- data.frame(hid = indx.co, order = seq_along(indx.co)) # Create dataframe with matched control units ordered
# Merge master data with this to get information on matched control group units, in the same order as the matching
data1_ordered <- left_join(order_df, data1, by = "hid") %>% arrange(order)
data.co <- data1_ordered

# covariates we would like to have balance on 
Btr <- data.tr[,c("elv_sh", "slope", "infants", "children", "adults")] %>% as.data.frame()  
Bco <- data.co[,c("elv_sh", "slope", "infants", "children", "adults")]  

  Nt = nrow(Btr) # Number of observations in Treatment Group
  Nc = nrow(Bco)
  
  colnames = c("Var" , "MeanT", "MeanC", "Pval ttest", "Var ratio", "Pval KS", "QQ med diff")
  # Var = Variable, MeanT = Mean for Treatment Group, MeanC = Mean for Control Group, Pval ttest = Pvalue from t-test, Var ratio = Ratio of variances between TG and CG, Pval KS = P-value from Kolmogorov-Smirnov tesst, QQ med diff = Quantile-Quantile median difference. 
  resbal = matrix(NA, nrow=ncol(Btr)*2 + 1, ncol = 7, dimnames = list(NULL, colnames)) # empty matrix to store results
  
  for(i in 1:ncol(Btr)) { # looping over each covariate in balance test
    
    # cat("Calculating balance tests for variable", i, "of", ncol(Btr), "\n")
    indx = (!is.na(Btr[,i]) & !is.na(Bco[,i]))
    Nmiss = sum(!indx)            
    xtr  = Btr[indx,i]
    xco  = Bco[indx,i]
    
    bal <- balanceUV(xtr,xco, ks=TRUE,nboots = 1000, paired=TRUE, match=FALSE)
    
    sigmat<-sqrt(bal$var.Tr)      
    sigmac<-sqrt(bal$var.Co)
    #sigmaboth<-sqrt(var(c(Btr[,i],Bco[,i]))*(1/Nt+1/Nc))
    
    resbal[2*(i-1)+1,1] <- names(Btr)[i]
    resbal[2*(i-1)+1,2] <- round(bal$mean.Tr,digits=3)
    resbal[2*(i-1)+2,2] <- paste("(",round(sigmat/sqrt(Nt),digits=3),")",sep="")
    resbal[2*(i-1)+1,3] <- round(bal$mean.Co,digits=3)
    resbal[2*(i-1)+2,3] <- paste("(",round(sigmac/sqrt(Nc),digits=3),")",sep="")   # st err of mean.Co
    resbal[2*(i-1)+1,4] <- round(bal$p.value,digits=3)
    resbal[2*(i-1)+1,5] <- round(bal$var.ratio,digits=3)
    resbal[2*(i-1)+1,6] <- round(bal$ks$ks.boot.pvalue,digits=3)  # bootstrapped KS  pvalue
    resbal[2*(i-1)+1,7] <- round(bal$qqsummary$mediandiff,digits=3)  # bootstrapped KS  pvalue
  }
  
  resbal[ncol(Btr)*2 + 1, 1] <- "Sample size (T,C,miss)"
  resbal[ncol(Btr)*2 + 1, 2] <- Nt
  resbal[ncol(Btr)*2 + 1, 3] <- Nc
  resbal[ncol(Btr)*2 + 1, 4] <- Nmiss
  
  print(resbal)
   
saveRDS(resbal, file="output/post-matching_balance_test.rds")


# Comparing tests: ------
`post-matching_balance_test`

