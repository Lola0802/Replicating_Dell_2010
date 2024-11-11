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

