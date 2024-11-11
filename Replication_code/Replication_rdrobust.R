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

