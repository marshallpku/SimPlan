# Importing termination rates in TPAF Experience Study 2012
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

# Assumptions about retirement age
r_min <- 40 # Need to find a more reasonable assumption
r_max <- 81 # Because mortality for active is given only up to age 80

age_full <- 55 # age eligible for full retirement benefit
yos_full <- 25 # yos eligible for full retirement benefit
age_sr   <- 60 # age eligible for service retirement (full benefit without yos requirement)

ea_min <- 20 # Because termination and disability rates start with 25
ea_max <- 59

yos_max <- r_max - ea_min # max possible yos


## Notes
  # Once  an  employee  becomes  eligible  for  reduced  or  unreduced  retirement,  the assumption  is  that  
    # the  member  will  no  longer  leave  under  the  withdrawal  decrement.  He  or  she  will  only  retire  
    # as  a  healthy  retiree,  disabled  retiree  or  die  during  active service. 
  # Problem: The eligible yos is 30 for class G, while the termination rates are only given up to 24(good for class A-F). 
  # Problem: The full retirement age is 62 for class E, F, and 65 for class G. while the termination rates are given only up to 59. 
  # Problem: termination rates for age less than 25 are missing from the table. For now, assume they are equal to the rates of age 25

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Load and reorganize original data         #######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

termMale1_raw <- read.xls(paste0(data.path, data.file), sheet = "TermMale1", skip = 3, na.strings = "NA", header = TRUE)
termMale2_raw <- read.xls(paste0(data.path, data.file), sheet = "TermMale2", skip = 3, na.strings = "NA", header = TRUE)
termFemale1_raw <- read.xls(paste0(data.path, data.file), sheet = "TermFemale1", skip = 3, na.strings = "NA", header = TRUE)
termFemale2_raw <- read.xls(paste0(data.path, data.file), sheet = "TermFemale2", skip = 3, na.strings = "NA", header = TRUE)


# Transpose termMale1_raw
termMale1_raw <- data.frame(age = 20:64, matrix(t(termMale1_raw)[2,], 64 - 20 + 1, 10, byrow = TRUE))


# Transpose termFemale1_raw
termFemale1_raw <- t(termFemale1_raw)[-1,] %>% data.frame %>% mutate(age = 39:40)
termFemale1_raw <- data.frame(age = c(rep(39,20), rep(40, 25))) %>% left_join(termFemale1_raw) %>% as.matrix


# Expand the raw data to age 20-24 and 60-64
male.top    <- data.frame(age = rep(25,5)) %>% left_join(termMale2_raw %>% filter(age == 25)) %>% mutate(age = 20:24)
male.bottom <- data.frame(age = rep(59,5)) %>% left_join(termMale2_raw %>% filter(age == 59)) %>% mutate(age = 60:64)
termMale2_raw <- rbind.fill(list(male.top, termMale2_raw, male.bottom))

female.top    <- data.frame(age = rep(25,5)) %>% left_join(termFemale2_raw %>% filter(age == 25)) %>% mutate(age = 20:24)
female.bottom <- data.frame(age = rep(59,5)) %>% left_join(termFemale2_raw %>% filter(age == 59)) %>% mutate(age = 60:64)
termFemale2_raw <- rbind.fill(list(female.top, termFemale2_raw, female.bottom))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Create general table                     #######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# To make the termination table compatible for all class, the range for age and yos are set below: 
  # Range of age 20 to 64;
  # Range of yos 0 to 29
  term_mat <- matrix(NA, 64 - 20 + 1, 29 + 1) %>% data.frame
  rownames(term_mat) <- 20:64
  colnames(term_mat) <- 0:29

  termMale_rf   <- term_mat 
  termMale_vest <- term_mat 
  termFemale_rf   <- term_mat 
  termFemale_vest <- term_mat 
  
  
  
  # Termination with refund, male
  termMale_rf[,1:10] <- termMale1_raw[, -1]         # yos: 0-9
  termMale_rf[,11:15] <- termMale2_raw$qxt.rf.10_14 # yos: 10-14
  termMale_rf[,16:20] <- termMale2_raw$qxt.rf.15_19 # yos: 15-19
  termMale_rf[,21:25] <- termMale2_raw$qxt.rf.20_24 # yos: 20-24
  termMale_rf[,26:30] <- termMale2_raw$qxt.rf.20_24 # yos: 25-26, same as yos 20-24  
  
  
  # Termination vested, male
  termMale_vest[,1:10]  <- 0                            # yos: 0-9
  termMale_vest[,11:15] <- termMale2_raw$qxt.vest.10_14 # yos: 10-14
  termMale_vest[,16:20] <- termMale2_raw$qxt.vest.15_19 # yos: 15-19
  termMale_vest[,21:25] <- termMale2_raw$qxt.vest.20_24 # yos: 20-24
  termMale_vest[,26:30] <- termMale2_raw$qxt.vest.20_24 # yos: 25-26, same as yos 20-24  
  
  # Termination with refund, female
  termFemale_rf[,1:10]  <- termFemale1_raw[, -1]         # yos: 0-9
  termFemale_rf[,11:15] <- termFemale2_raw$qxt.rf.10_14 # yos: 10-14
  termFemale_rf[,16:20] <- termFemale2_raw$qxt.rf.15_19 # yos: 15-19
  termFemale_rf[,21:25] <- termFemale2_raw$qxt.rf.20_24 # yos: 20-24
  termFemale_rf[,26:30] <- termFemale2_raw$qxt.rf.20_24 # yos: 25-26, same as yos 20-24  
  
  # Termination vested,female
  termFemale_vest[,1:10]  <- 0                            # yos: 0-9
  termFemale_vest[,11:15] <- termFemale2_raw$qxt.vest.10_14 # yos: 10-14
  termFemale_vest[,16:20] <- termFemale2_raw$qxt.vest.15_19 # yos: 15-19
  termFemale_vest[,21:25] <- termFemale2_raw$qxt.vest.20_24 # yos: 20-24
  termFemale_vest[,26:30] <- termFemale2_raw$qxt.vest.20_24 # yos: 25-26, same as yos 20-24  
  
  
  
  # Convert to long form
  get_long_qxt <- function(df, value.name){
     df %<>% mutate(age = 20:64) %>% 
      gather_("yos", value.name, gather_col = as.character(0:29)) %>% 
      mutate(yos = levels(yos)[yos] %>% as.numeric,
             ea  = age - yos) %>% 
      filter(ea >= 20) # drop ea less than 20
    return(df)
  }  
   
  termMale_rf     <-  get_long_qxt(termMale_rf,   "qxt.rf")
  termMale_vest   <-  get_long_qxt(termMale_vest, "qxt.vest")
  termFemale_rf   <-  get_long_qxt(termFemale_rf, "qxt.rf")
  termFemale_vest <-  get_long_qxt(termFemale_vest, "qxt.vest")
  
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                   Create class specific tables                   #######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# The experience study assume that the termination rates are zero for those who are eligible for full or
# reduced retirement benefits. Since the eligibility criteria vary across classes, we need to produce class specific tables.

## Class A/B
  # term rates are 0 for 
   # yos >= 25
   # yos < 25 and age >= 60

  termMale_AB_rf     <- termMale_rf   %>% mutate(qxt.rf   = ifelse( yos >= 25 | (yos < 25 & age >=60),0, qxt.rf))
  termMale_AB_vest   <- termMale_vest %>% mutate(qxt.vest = ifelse( yos >= 25 | (yos < 25 & age >=60),0, qxt.vest))
  termFemale_AB_rf   <- termFemale_rf %>% mutate(qxt.rf   = ifelse( yos >= 25 | (yos < 25 & age >=60),0, qxt.rf))
  termFemale_AB_vest <- termFemale_vest %>% mutate(qxt.vest = ifelse( yos >= 25 | (yos < 25 & age >=60),0, qxt.vest))
  
  # check results
  termMale_rf %>% select(-ea) %>% spread(yos, qxt.rf) # looks fine
  

#### Save results
save(termMale_AB_rf,   termMale_AB_vest, 
     termFemale_AB_rf, termFemale_AB_vest,
     file = paste0(data.path,"decrement_term.RData"))
  
  
  
  

  
  
  