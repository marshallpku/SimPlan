# Importing retirement rates in TPAF Experience Study 2012
# 3/19/2015
# Yimeng Yin


## Preamble ###############################################################

library(knitr)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(microbenchmark)
#library(xlsx)

data.path <- paste0(getwd(), '/Data/') 
data.file <- "TPAF ES2012 Assumptions.xlsx"

options(stringsAsFactors = FALSE)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Class AB                                   %%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retireMale_raw   <- read.xls(paste0(data.path, data.file), sheet = "RetireMale", skip = 4, na.strings = "NA", header = TRUE)
retireFemale_raw <- read.xls(paste0(data.path, data.file), sheet = "RetireFemale", skip = 4, na.strings = "NA", header = TRUE)
# read.xls can specify na.strings while read.xlsx cannot. 

# Assumptions about retirement age
r_min <- 40 # Need to find a more reasonable assumption
r_max <- 81 # Because mortality for active is given only up to age 80

age_full <- 55 # age eligible for full retirement benefit
yos_full <- 25 # yos eligible for full retirement benefit
age_sr   <- 60 # age eligible for service retirement (full benefit without yos requirement)

ea_min <- 20 # Because termination and disability rates start with 25
ea_max <- 59

yos_max <- r_max - ea_min # max possible yos


## Class AB Male, age by yos ####

# Notes
  # For all with yos < 25, retirement can only occur after full retirement age(60 for AB), retirement rates before that are 0. 

# Organize the raw table
retireMale_AB_raw <- data.frame(age = r_min:r_max) %>% 
  left_join(retireMale_raw %>% select(age, ends_with("AB"))) %>%
  mutate(qxr_gt25yos_red_AB = ifelse(age < age_full, qxr_lt25yos_AB, NA),
         # Rates for yos <= 25
         qxr_lt25yos_AB = ifelse(age >= age_sr, qxr_lt25yos_AB, 0),
         qxr_lt25yos_AB = ifelse(age >= 71, qxr_lt25yos_AB[age == 71], qxr_lt25yos_AB),
         # Rates for yos >= 25 and reduced benefits
         qxr_gt25yos_red_AB = ifelse(age <= 47, qxr_gt25yos_red_AB[age == 47], qxr_gt25yos_red_AB),
         # Rates for yos >= 25 and reduced benefits
         qxr_gt25yos_f_AB = ifelse(age >= 71, qxr_gt25yos_f_AB[age == 71], qxr_gt25yos_f_AB),
         qxr_gt25yos_AB   = ifelse(age >= 71, qxr_gt25yos_AB[age == 71], qxr_gt25yos_AB)
         )

# Create a data frame for the decrement table  
retireMale_AB <- matrix(NA, nrow = r_max - r_min + 1, ncol = yos_max ) # row for age; col for yos
dimnames(retireMale_AB) <- list(r_min:r_max, 1:yos_max)


# Fill in rates for yos < 25 (Normal retirment with full benefits )
retireMale_AB[, 1:(yos_full - 1)] <- retireMale_AB_raw$qxr_lt25yos_AB

# Fill in rates for yos > 25 but age < 55 (Early retirement with reduced benefits )
retireMale_AB[, yos_full:yos_max] <- retireMale_AB_raw$qxr_gt25yos_red_AB

# Fill in rates for those who first achieve yos = 25 or age 55
retireMale_AB[as.numeric(rownames(retireMale_AB)) >= age_full, yos_full] <- retireMale_AB_raw$qxr_gt25yos_f_AB[as.numeric(rownames(retireMale_AB)) >= age_full]
retireMale_AB[as.numeric(rownames(retireMale_AB)) == age_full, yos_full:yos_max] <- retireMale_AB_raw$qxr_gt25yos_f_AB[as.numeric(rownames(retireMale_AB)) == age_full]

# Fill in rates for yos >=25 and age >=55
retireMale_AB[as.numeric(rownames(retireMale_AB)) > age_full, (yos_full + 1):yos_max]<- retireMale_AB_raw$qxr_gt25yos_AB[as.numeric(rownames(retireMale_AB)) > age_full]

# All active are assumed to retire at r_max
retireMale_AB[as.numeric(rownames(retireMale_AB)) == r_max,] = 1


# Assign NA to impossible combinations of age and yos: entry age < 20, i.e. age - yos < 20
bad_ea <- t(sapply(r_min:r_max, function(x) x - (1:yos_max))) < ea_min
retireMale_AB[bad_ea] <- NA


# Convert to data frame
retireMale_AB <- data.frame(age = r_min:r_max, retireMale_AB) %>% 
  gather(yos, qxr, -age) %>% 
  mutate(yos = gsub("[^0-9]", "", yos) %>% as.numeric,
         ea  = age - yos) %>% 
  filter(ea >= ea_min, ea <= ea_max) %>% 
  select(ea, age, everything()) %>% 
  arrange(ea, age)

# x <- retireMale_AB1 %>% select(-yos) %>% spread(ea, value)


## Class AB Female, age by yos ####

# Organize the raw table
retireFemale_AB_raw <- data.frame(age = r_min:r_max) %>% 
  left_join(retireFemale_raw %>% select(age, ends_with("AB"))) %>%
  mutate(qxr_gt25yos_red_AB = ifelse(age < age_full, qxr_lt25yos_AB, NA),
         # Rates for yos <= 25
         qxr_lt25yos_AB = ifelse(age >= age_sr, qxr_lt25yos_AB, 0),
         qxr_lt25yos_AB = ifelse(age >= 71, qxr_lt25yos_AB[age == 71], qxr_lt25yos_AB),
         # Rates for yos >= 25 and reduced benefits
         qxr_gt25yos_red_AB = ifelse(age <= 47, qxr_gt25yos_red_AB[age == 47], qxr_gt25yos_red_AB),
         # Rates for yos >= 25 and reduced benefits
         qxr_gt25yos_f_AB = ifelse(age >= 71, qxr_gt25yos_f_AB[age == 71], qxr_gt25yos_f_AB),
         qxr_gt25yos_AB   = ifelse(age >= 71, qxr_gt25yos_AB[age == 71], qxr_gt25yos_AB)
  )

# Create a data frame for the decrement table  
retireFemale_AB <- matrix(NA, nrow = r_max - r_min + 1, ncol = yos_max ) # row for age; col for yos
dimnames(retireFemale_AB) <- list(r_min:r_max, 1:yos_max)


# Fill in rates for yos < 25 (Normal retirment with full benefits )
retireFemale_AB[, 1:(yos_full - 1)] <- retireFemale_AB_raw$qxr_lt25yos_AB

# Fill in rates for yos > 25 but age < 55 (Early retirement with reduced benefits )
retireFemale_AB[, yos_full:yos_max] <- retireFemale_AB_raw$qxr_gt25yos_red_AB

# Fill in rates for those who first achieve yos = 25 or age 55
retireFemale_AB[as.numeric(rownames(retireFemale_AB)) >= age_full, yos_full] <- retireFemale_AB_raw$qxr_gt25yos_f_AB[as.numeric(rownames(retireFemale_AB)) >= age_full]
retireFemale_AB[as.numeric(rownames(retireFemale_AB)) == age_full, yos_full:yos_max] <- retireFemale_AB_raw$qxr_gt25yos_f_AB[as.numeric(rownames(retireFemale_AB)) == age_full]

# Fill in rates for yos >=25 and age >=55
retireFemale_AB[as.numeric(rownames(retireFemale_AB)) > age_full, (yos_full + 1):yos_max]<- retireFemale_AB_raw$qxr_gt25yos_AB[as.numeric(rownames(retireFemale_AB)) > age_full]


# All active are assumed to retire at r_max
retireFemale_AB[as.numeric(rownames(retireFemale_AB)) == r_max,] = 1


# Assign NA to impossible combinations of age and yos: entry age < 20, i.e. age - yos < 20
bad_ea <- t(sapply(r_min:r_max, function(x) x - (1:yos_max))) < ea_min
retireFemale_AB[bad_ea] <- NA


# Convert to data frame
retireFemale_AB <- data.frame(age = r_min:r_max, retireFemale_AB) %>% 
  gather(yos, qxr, -age) %>% 
  mutate(yos = gsub("[^0-9]", "", yos) %>% as.numeric,
         ea  = age - yos) %>% 
  filter(ea >= ea_min, ea <= ea_max) %>% 
  select(ea, age, everything()) %>% 
  arrange(ea, age)




## Save results
save(retireMale_AB, retireFemale_AB,  file = paste0(data.path, "decrement_retire.RData"))











