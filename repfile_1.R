# Replication Project for Causal Inference Exam # 

# Goal: Replicate and Extend "The Persisent Effects of Peru's Mining Mita" by Melissa Dell, published in Econometrica in 2010. 

# Author: Lolakshi Rajlakshmi (12339702)

# This project consists of 3 files, to be run in the following order
# 1. repfile_1 : Recreate main tables and figures from Dell 2010. 
# 2. 
# 3. extendfile_1 : Extend analysis 

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

# (1) Replicating Table 1: Summary Statistics -----------------------------------
# The Stata program for creating Table 1 is maketable1.do. The data used to make this table are: gis grid.dta, consumption.dta, spec check1572.dta, and budget1572.dta. The file consumption.dta is produced by the program maketable2.do, described below, and the files spec check1572.dta and budget1572.dta are produced by the program maketable5.do, also described below. Hence, these programs must be run before maketable1.do in order for it to function.

# Loading the data 
gis_grid
consumption # Run maketable2.do to get this
spec_check1572 # Run maketable5.do to get this
budget1572 #  Run maketable5.do to get this

--------------------------------------------------------------------------------------------------------------------------------------------


# 4 1572 Tribute and Population (Table 5) -----------------------------------
# The Stata program for creating Table 5 is maketable5.do. 

# Required data: 
# Tribute and Population data from 1572
tribute1572.dta, 
1572budget.csv, and 
1572demo- graphic.csv
# Geographic data
gis dist.dta
# 5 Haciendas (Table 6) -----------------------------------
# The Stata program for creating Table 6 is maketable6.do. 

# Required data:
# Haciendas and land distribution
 hacienda1689.csv, 
 hacienda1845.csv, 
 hacienda1940.csv, 
 apurimac-d2.dta, 
 arequipa-d2.dta, 
 ayacucho-d2.dta, 
 cusco-d2.dta, and 
 puno- d2.dta. 
 # Geographic data
gis dist.dta

# 6 Education (Table 7) -----------------------------------
# The Stata program for creating Table 7 is maketable7.do. 
# Required data:
#Education 
educ1876.csv, 
educ1940.csv, and 
enaho01a_2001iv_300.dta.
# Geographic data
gis dist.dta

# 7 Roads (Table 8) -----------------------------------
# The Stata program for creating Table 8 is maketable8.do. 
# Required data:
# Roads
distr.dbf, 
vecd‘X’.dbf and 
depd‘X’.dbf, where ‘X’=1/5, and 9. These .dbf files can, in turn, be reproduced by the program file roads.py, included and described at the bottom of this document. The program also uses the data file gis dist.dta.
# Geographic data
gis dist.dta

# 9 Appendix (Table A3) - BIG!! maybe-----------------------------------
# The Stata program for creating Table A3 is maketableA3.do. The data files used by this program are produced by the programs maketable2 through maketable9, and are hacienda1689.dta, hacienda1845.dta, hacienda1940.dta, educ1876.dta, educ1940.dta, educ2001.dta, roads.dta, and ag mkts1994.dta.

# NOT making: -----------------------------------
# 8 Consumption Channels (Table 9)
# The Stata program for creating Table 9 is maketable9.do. The data files on occupation, agricultural market participation, and the agricultural labor force used to make this table are: occupation1993.csv, apurimac-d2.dta, arequipa-d2.dta, ayacucho-d2.dta, cusco-d2.dta, puno-d2.dta, apurimac-d4.dta, arequipa-d4.dta, ayacucho-d4.dta, cusco-d4.dta, and puno- d4.dta. The program also uses the data file gis dist.dta.

# 10 Maps (Figures 1 and A1)
# The mapping (.mxd), shape, and raster files necessary for reproducing the maps in Figures 1 and A1 are in the folder Maps1 A1 Arc. Load the files Figure1.mxd and FigureA1.mxd into ArcGIS and make sure that the settings on your machine are such that relative directory paths - as opposed to the full paths - are used. Each map will display once its .mxd file is loaded.

# 11 RD Figures
# The program, shape, and data files necessary for reproducing the RD figures (Figures 2, 3, and A2) are contained in the folder RDfigs. To reproduce the figures, run the file mita run.R, using the statistical package R.


