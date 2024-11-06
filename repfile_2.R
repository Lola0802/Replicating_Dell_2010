# (3) Living Standards (Tables 2 through 4) -----------------------------------
rm(list = ls())

# Required data: 
# Consumption data
household_data <- read_dta("replication_data/enaho01_2001iv_200.dta")
summary_data <- read_dta("replication_data/sumaria_2001iv.dta")
language_data <- read_dta("replication_data/enaho01b_2001iv.dta")
# Height data
ctalla.dta
# GIS data
gis_data <- read_dta("replication_data/gis_dist.dta")
nrivers.csv # for table 4 only 


# Output created: consumption and height data

# NOTE: Replicate either Table 3 or Table 4
# The Stata programs for creating Tables 2 through 4 are maketable2.do, maketable3.do, and maketable4.do, respectively. To produce Tables 3 and 4, you must first run the program maketable2, as Tables 3 and 4 use data files created by this program.

# (3.1) Table 2: Main impacts of Mita on household consumption and stunting of growth in children

# Clean household data
household_data <- household_data %>%
  select(conglome, vivienda, hogar, ubigeo, p203, p207, p208a, p208b) %>% # Get household and identifier codes and data on age and sex of members
  filter(p203 != 0) %>% # Filter out people who were in the panel before but not in household anymore
  mutate(
    p208b = ifelse(is.na(p208b), 0, p208b),
    age = p208a + (p208b / 12),
    hhead = if_else(p203 >= 2 & p203 <= 10, 0, p203),
    male = if_else(p207 == 2, 0, p207)
  ) %>%
  select(-p208a, -p208b, -p203, -p207 )

# Age frequency distribution
View(household_data %>% group_by(age) %>% summarise(n = n())) 

# Add labels to variables
household_data <- household_data %>%
  set_variable_labels(
    conglome = "conglomerado",
    vivienda = "vivienda",
    hogar = "hogar",
    ubigeo = "ubicacion geografica",
    hhead = "household head",
    male = "male"
  )

# Define labels
noyes <- c("no" = 0, "yes" = 1)
gender <- c("female" = 0, "male" = 1)

# Apply value labels
household_data <- household_data %>%
  mutate(hhead = as_factor(hhead, labels = noyes),
         male = as_factor(male, labels = gender))

# Extract geographical codes and keep departments in sample
household_data <- household_data %>%
  mutate(
    ccdd = substr(ubigeo, 1, 2),
    ccpp = substr(ubigeo, 3, 2),
    ccdi = substr(ubigeo, 5, 2)
  ) %>%
  mutate_at(vars(ccdd, ccpp, ccdi), as.numeric) %>%
  filter(ccdd %in% c(3, 4, 5, 8, 21))

# HOUSEHOLD CONSUMPTION NET TRANSFERS PER EQUIVALENT MEMBER

# Create household consumption and demographic variables
household_data <- household_data %>%
  mutate(hid = conglome + vivienda + hogar,
         var1 = 1) %>% # Generate a unique household identifier
  group_by(hid) %>%
  mutate(hhmem = sum(var1)) %>% # create a variable which contains the number of members per household
  ungroup() %>%
  mutate(lhhmem = log(hhmem), 
         age_cat = case_when( 
           age >= 0 & age <= 14 ~ 1,
           age >= 15 ~ 2 # Create an age category variable
         ),# Have to add a variable `kids` that counts number of children within each household i.e, groupby hid
         k_hhmem = sum(age_cat == 1) / hhmem,
         # Have to create another variable age_cat2 or drop out age_cat and recode 
         kids = sum(age_cat == 1), 
         children = sum(age_cat == 2),
         adults = sum(age_cat == 3), # I don't think this will work, because age_Cat has 2 levels. create enother variable or create ces as a case when?
         ces = sum(replace(age_cat, age_cat == 1, 0.4), # calculates the consumption equivalence scale (denoted as ces), which adjusts household consumption based on the number and age composition of its members
                   replace(age_cat, age_cat == 2, 0.5), 
                   replace(age_cat, age_cat == 3, 1))
  )
  #  %>% select(-age_cat, - age_cat2)

# Save the updated data
write_dta(household_data, "consumption.dta")

# Load and clean summary data for household consumption
summary_data <- summary_data%>%
  select(conglome, vivienda, hogar, ubigeo, factorto, gashog2d, ingtexhd, ingtrahd,
         gru13hd1, gru13hd2, gru13hd3, gru23hd1, gru23hd2, gru33hd1, gru33hd2,
         gru43hd1, gru43hd2, gru53hd1, gru53hd2, gru63hd1, gru63hd2, gru73hd1,
         gru73hd2, gru83hd1, gru83hd2)

# Recode missing values to 0 for transfer-related variables
summary_data <- summary_data %>%
  mutate_at(vars(ingtexhd, ingtrahd, starts_with("gru")), ~ ifelse(is.na(.), 0, .))

# Merge household data and summary data
merged_data <- household_data %>%
  inner_join(summary_data, by = c("conglome", "vivienda", "hogar"))

# Generate household consumption variable
merged_data <- merged_data %>%
  mutate(hconsump = gashog2d - rowSums(select(., starts_with("gru"))), # I think gashog2d has to be defined first? hmm, check data # generated household consumption in normal prices
         hconsumplm = hconsump / defesp, # in the line above, ingtexhd and ingtrahd also have to be removed? # gen hh consumption in lima metropolitan prices
         hhequiv = hconsumplm / ces, 
         # for Deaton values
         lhhequiv = log(hhequiv), 
         lhhconsplm = log(hconsumplm)) # log household consumption

# keep only hh heads  %>% filter(hhead == 1)

# Save final dataset
write_dta(merged_data, "consumption.dta")

# *Definition of aggregate household consumption
# /*GASHOG2D = G05HD + IG06HD + G07HD + IG08HD + SG23 + SIG24 +
# GRU11HD + GRU12HD1 + GRU12HD2 + GRU13HD1 +
# GRU13HD2 + GRU13HD3 + GRU21HD + GRU22HD1 +
# GRU22HD2 + GRU23HD1 + GRU23HD2 + GRU23HD3 +
# GRU24HD + GRU31HD + GRU32HD1 + GRU32HD2 +
# GRU33HD1 + GRU33HD2 + GRU33HD3 + GRU34HD +
# GRU41HD + GRU42HD1 + GRU42HD2 + GRU43HD1 +
# GRU43HD2 + GRU43HD3 + GRU44HD + GRU51HD +
# GRU52HD1 + GRU52HD2 + GRU53HD1 + GRU53HD2 +
# GRU53HD3 + GRU54HD + GRU61HD + GRU62HD1 +
# GRU62HD2 + GRU63HD1 + GRU63HD2 + GRU63HD3 +
# GRU64HD + GRU71HD + GRU72HD1 + GRU72HD2 +
# GRU73HD1 + GRU73HD2 + GRU73HD3 + GRU74HD +
# GRU81HD + GRU82HD1 + GRU82HD2 + GRU83HD1 +
# GRU83HD2 + GRU83HD3 + GRU84HD*/

# /*transfers that I subtract from household consumption
# INGTEXHD - Ingreso por transferencias corrientes del extranjero
# INGTRAHD - Ingreso por transferencias corrientes monetarias del país
# GRU13HD1 - Alimentos - Donación Público
# GRU13HD2 - Alimentos - Donación Privada
# GRU13HD3 - alimentos donado, regelado, etc
# GRU23HD1 - vestido y calzado, donacion publica
# GRU23HD2 - vestido y calzado, donacion privada
# GRU33HD1 - Alquiler de Vivienda, Combustible, Electricidad y Conservación de la
# Vivienda - Donación Pública
# GRU33HD2 - Alquiler de Vivienda, Combustible, Electricidad y Conservación de la
# Vivienda - Donación Privada
# GRU43HD1 - Muebles y Enseres, y Mantenimiento de la Vivienda - Donación
# Pública
# GRU43HD2 - Muebles y Enseres, y Mantenimiento de la Vivienda - Donación
# Privada
# GRU53HD1 - Cuidado, Conservación de la Salud y Servicios Médicos - Donación
# Pública
# GRU53HD2 - Cuidado, Conservación de la Salud y Servicios Médicos - Donación
# Privada
# GRU63HD1 - Transportes y Comunicaciones - Donación Pública
# GRU63HD2 - Transportes y Comunicaciones - Donación Privada
# GRU73HD1 - Esparcimiento, Diversión, Servicios Culturales y de Enseñanza -
# Donado Público
# GRU73HD2 - Esparcimiento, Diversión, Servicios Culturales y de Enseñanza -
# Donado Privado
# GRU83HD1 - Otros Bienes y Servicios - Donación Pública
# GRU83HD2 - Otros Bienes y Servicios - Donación Privada
# gashog2d	gasto total trimestral
# defesp	    deflactor espacial de precios base a lima metropolitana*/



# Load and clean data related to language and ethnicity
language_data <- read_dta("enaho01b_2001iv.dta") %>%
  select(conglome, vivienda, hogar, ubigeo, q21, q24, q25, q231, q232, q233, q28, q301, q302, q303) %>%
  mutate(ccdd = as.numeric(substr(ubigeo, 1, 2))) %>%
  filter(ccdd %in% c(3, 4, 5, 8, 21)) %>%
  # Which language is spoken most frequently
  mutate(CAST = ifelse(q21 == 1 | q24 == 1, 1, 0),
         QUE = ifelse(q21 == 2 | q24 == 2, 1, 0),
         AYM = ifelse(q21 == 3 | q24 == 3, 1, 0))

# Merge with household data
final_data <- household_data %>%
  inner_join(language_data, by = c("conglome", "vivienda", "hogar"))
# Dell creates tables where she sees which 

# Save final dataset
write_dta(final_data, "consumption.dta")

# Regression analysis
# Load GIS data and merge with final_data
gis_data <- read_dta("gis_dist.dta") %>%
  mutate(ubigeo = as.numeric(ubigeo))

final_data <- final_data %>%
  inner_join(gis_data, by = "ubigeo")

# Regression with cubic polynomial in latitude and longitude
model1 <- lm(lhhequiv ~ pothuan_mita + x + y + I(x^2) + I(y^2) + I(x*y) +
               I(x^3) + I(y^3) + I(x^2*y) + I(x*y^2) + 
               infants + children + adults + elv_sh + slope + bfe4, 
             data = final_data %>% filter(cusco != 1 & d_bnd < 100))
summary(model1)

# Save regression results
write.csv(broom::tidy(model1), "table2a.csv")

# (3.2) Table 3: Robustness to using single dimensional RDD polynomial - specification tests

# (3.3) Table 4: Robustness to factors other than functional form of RDD polynomial
