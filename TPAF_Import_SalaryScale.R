# Importing salary scale in TPAF Experience Study 2012
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

source("E:/GitHub/PenSim-Projects/Learn-PenSim/Functions.R")

data.path <- paste0(getwd(), '/Data/') 
data.file <- "TPAF ES2012 Assumptions.xlsx"

options(stringsAsFactors = FALSE)


age_min <- 20
age_active_max <- 80 # Because mortality for active is given only up to age 80

r_min <- 40 # Need to find a more reasonable assumption
r_max <- 81 # Because mortality for active is given only up to age 80

age_full <- 55 # age eligible for full retirement benefit
yos_full <- 25 # yos eligible for full retirement benefit
age_sr   <- 60 # age eligible for service retirement (full benefit without yos requirement)

ea_min <- age_min # Because termination and disability rates start with 25
ea_max <- 59

yos_max <- r_max - ea_min # max possible yos



#Notes
 # The maximum year needed: 2013 + 61 = 2074, a 2013 entrant at age 20 will reach the max yos=61 (20 ~ 80) in 2074.
 # The minimum year needed: 2013 - 61 + 1 = 1953, a 1953 entrant at age 20 will reach the max yos=61 (20 ~ 80) in 2013.
 # The maximum yos needed:  61
 # scales for yos > 40 are missing, for now we assume the scales are the same as yos 40.  
 # scales before 2009 are missing, for now we assume the scales are the same as those in 2009.


#****************************************************************************
#
#                           Creat salary scale ####
#
#****************************************************************************

salaryScale_raw <- read.xls(paste0(data.path, data.file), sheet = "Salary Growth", skip = 3, na.strings = "NA", header = TRUE)
salaryScale_append <- data.frame(yos = 41:61,
                                 matrix(salaryScale_raw[salaryScale_raw$yos == 40, -1] %>% as.numeric, 21, 8, byrow = T))

colnames(salaryScale_append) <- colnames(salaryScale_raw)
salaryScale_raw <- rbind(salaryScale_raw, salaryScale_append)



salaryScale <- matrix(NA, nrow = 62, ncol = 2074 - 1953 + 1) %>% data.frame
colnames(salaryScale) <- 1953:2074

salaryScale[,1:(2009 - 1953 + 1)] <- salaryScale_raw$X2009
salaryScale[,58:61] <- salaryScale_raw %>% select(X2010:X2013)
salaryScale[,62:64] <- salaryScale_raw$X2014.2016
salaryScale[,65:69] <- salaryScale_raw$X2017.2021
salaryScale[,70:ncol(salaryScale)] <- salaryScale_raw$gt2021

salaryScale <- mutate(salaryScale, yos = 0:61) %>% 
  gather(year, growth, -yos) %>% 
  mutate(year = levels(year)[year] %>% as.numeric)



#****************************************************************************
#
#                   Imputation of salary along yos ####
#
#****************************************************************************


# assume the salary scale across yos 0-44 is proportional ot the salary scale across age 20-64. 
# We impute the average salary of each combination of age and yos, maintaining the average annual salary by age
# given in AV 2013.

load("Data/census_active.RData")
load("Data/salary13_active.RData")

get_salary <- function(census_data, salary_data){
# function "splong" must be loaded  
  
W <- census_data %>% select(-age) %>% as.matrix
S13.yos <- salary_data[1:ncol(W),"Salary"]
S13.age <- salary_data[1:nrow(W),"Salary"]

A <- (S13.age*rowSums(W)) / (W %*% S13.yos) # adjustment factor
Salary <- A %x% t(salary_data$Salary) # salary matrix

# check the correctness
(rowSums(W * Salary[,1:ncol(W)]) / rowSums(W) -  S13.age) %>% sum %>% print # error is acceptable

# Convert to long form
Salary <-  data.frame(age = census_data$age, Salary) 
colnames(Salary) <- c("age", 0:(yos_max - 1))  
Salary %<>% gather(yos, Salary, -age) 
Salary <-  splong(Salary, "age", 1:80) %>% 
  mutate(year = 2013,
         yos = levels(yos)[yos] %>% as.numeric) %>% 
  filter(age - yos >= 20)

return(Salary)
}

salary_ActContFemale <- get_salary(census_ActContFemale, salary_ActContFemale)
salary_ActContMale   <- get_salary(census_ActContMale, salary_ActContMale)
salary_ActNcontFemale <- get_salary(census_ActNcontFemale, salary_ActNcontFemale)
salary_ActNcontMale   <- get_salary(census_ActNcontMale, salary_ActNcontMale)



#**********************************************************************
#
#                  Create a complete salary scale ####
#
#**********************************************************************
SS <- expand.grid(start.year = 1953:2013, ea = age_min:age_active_max, age = age_min:age_active_max) %>%
  mutate(yos  = age - ea,
         year = start.year + yos,
         include = start.year + age_active_max - ea >= 2013
         ) %>%
  filter(age >= ea, include == TRUE) %>% 
  left_join(salaryScale) %>% 
  arrange(start.year, ea, age) %>%
  group_by(start.year, ea) %>% 
  mutate(scale = ifelse(year == min(year), 1, cumprod(1+growth) %>% lag),
         scale13 = scale/scale[year == 2013]) %>% # scale relative to 2013 salary
  select(-include) 

SS$growth %>% is.na %>% sum # check if all cells are filled


# load 2013 salary

SS_ActConMale <- SS %>% left_join(salary_ActContMale) %>% 
  mutate(sx = scale13 * Salary[year == 2013])

SS_ActConFemale <- SS %>% left_join(salary_ActContFemale) %>% 
  mutate(sx = scale13 * Salary[year == 2013])

SS_ActNconMale <- SS %>% left_join(salary_ActNcontMale) %>% 
  mutate(sx = scale13 * Salary[year == 2013])

SS_ActNconFemale <- SS %>% left_join(salary_ActNcontFemale) %>% 
  mutate(sx = scale13 * Salary[year == 2013])

save(SS_ActConMale, SS_ActConFemale, SS_ActNconMale, SS_ActNconFemale, 
     file = paste0(getwd(), "/Data/salaryTable_active.RData"))











# 
# # Knonecker product
# # a <- 1:4
# # b <- c(2,2,2,2)
# # t(a) %x% b
# # a %x% t(b)
# 
# dim(W)
# 
# S13.age
# S13.yos
