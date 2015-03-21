# Importing disability rates in TPAF Experience Study 2012
# 3/21/2015
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
 # No disability rates for age under 25. Assume they are the same as the rates at 25
 # No disability rate for age 80. Assume they are the same as the rate at 79. 

## Load data
disb_raw <- read.xls(paste0(data.path, data.file), sheet = "Disability", skip = 3, na.strings = "NA", header = TRUE)

## Expand to age 20-24 and 80
top    <- data.frame(age = rep(25,5)) %>% left_join(disb_raw) %>% mutate(age = 20:24)
bottom <- data.frame(age = 79) %>% left_join(disb_raw)%>% mutate(age = 80)
disb_raw<- rbind.fill(list(top, disb_raw, bottom))

## Split rates for male and female
disbMale   <- select(disb_raw, age, qxd_o = qxd_o_m, qxd_a = qxd_a_m)
disbFemale <- select(disb_raw, age, qxd_o = qxd_o_f, qxd_a = qxd_a_f)










