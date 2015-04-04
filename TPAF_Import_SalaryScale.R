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


# Assume the salary scale across yos 0-44 is proportional to the salary scale across age 20-64. 
# We impute the average salary of each combination of age and yos, maintaining the average annual salary by age.

load("Data/census_active.RData")
load("Data/salary13_active.RData")

get_salary <- function(census_data, salary_data){

# function "splong" from Functions.R must be loaded  
  
W <- census_data %>% select(-age) %>% as.matrix # Census matrix: age(20~69) by yos(0~44)
S13.yos <- salary_data[1:ncol(W),"Salary"]      # Extract the salary for age 20~64, to match yos 0~44
S13.age <- salary_data[1:nrow(W),"Salary"]      # salary for age 20~69


# Since we assume salary scale across yos is proportional to salary scale across age, we need a factor to construct 
# salary along yos for each age. Let's define the vector of the factors as "A"(dim = 50, for age 20~69). We need to 
# solve for A, subject to that at each age x, the total salary computed from the average salary of age x is equal to 
# the total salary computed using the imputed salary across yos at age x. This constraint can be written as
# A*(W %*% S13.yos) = (S13.age*rowSums(W)). 

# Solve for vector A:
A <- (S13.age*rowSums(W)) / (W %*% S13.yos) # adjustment factor
# Calculate salary across yos for each age using A
# Note the use of knonecker product %x%: ith row in Salary is calculated as A[i] mulitplied by the row vector t(salary_data$Salary).
Salary <- A %x% t(salary_data$Salary) # salary matrix: age(20~64) by yos (0~60)


## Check the correctness
(rowSums(W * Salary[,1:ncol(W)]) / rowSums(W) -  S13.age) %>% sum %>% print # error is acceptable

# Convert to long form and extend the age span to 80(max age for actives) using spline smoothing function "splong".
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



## Check consistency with AV

check_results <- function(df_salary, df_census){
  
na2zero <- function(x){x[is.na(x)] <- 0 ;return(x)}

df <- df_salary %>% 
     left_join(df_census %>% gather(yos, Pop, -age) %>% mutate(yos=as.numeric(levels(yos)[yos]))) %>% 
     mutate(ea = age - yos) %>% 
     na2zero() 

value.avg <- c(age.avg =    weighted.mean(df$age,    w = df$Pop),
               yos.avg =    weighted.mean(df$yos,    w = df$Pop),
               ea.avg  =    weighted.mean(df$ea,     w = df$Pop),
               Salary.avg = weighted.mean(df$Salary, w = df$Pop))

payroll <- sum(df$Salary * df$Pop)

df_group <- df %>% 
  mutate(group = trunc(age/5)*5,
         Salary.sum = Salary * Pop) %>% 
  group_by(group) %>% 
  summarise_each(funs(sum), Salary.sum, Pop) %>% 
  mutate(Salary.ageAvg = Salary.sum/Pop) %>% 
  filter(group <=65) %>% 
  select(group, Pop, Salary.ageAvg)

return(list(value.avg = value.avg, df_group = df_group, payroll = payroll))
}

(check_ContFemale  <- check_results(salary_ActContFemale, census_ActContFemale))
(check_ContMale    <- check_results(salary_ActContMale, census_ActContMale))
(check_NcontFemale <- check_results(salary_ActNcontFemale, census_ActNcontFemale))
(check_NcontMale   <- check_results(salary_ActNcontMale, census_ActNcontMale))
# Summary statistics and average salary are quite close to the original values on AV page 49~50

(payroll.tot <- check_ContFemale$payroll  + check_ContMale$payroll +
               check_NcontFemale$payroll +  check_NcontMale$payroll) #10.715b
# total payroll is greater than the total "Appropriation Salary" of class AB on page 33 (8.236b)

# To further check it, we calculate total salary using original census data on AV p49~50
ContFemale.raw <- read.xls("Data/TPAF Census Data.xlsx", sheet = "ActContFemale", skip = 1, na.strings = "NA", header = TRUE)
ContMale.raw <- read.xls("Data/TPAF Census Data.xlsx", sheet = "ActContMale", skip = 1, na.strings = "NA", header = TRUE)
NcontFemale.raw <- read.xls("Data/TPAF Census Data.xlsx", sheet = "ActNcontFemale", skip = 1, na.strings = "NA", header = TRUE)
NcontMale.raw <- read.xls("Data/TPAF Census Data.xlsx", sheet = "ActNcontMale", skip = 1, na.strings = "NA", header = TRUE)

(payroll.original <- 
  sum(as.numeric(ContFemale.raw[-12,11] * ContFemale.raw[-12,12])) + 
  sum(as.numeric(ContMale.raw[-12,11]   * ContMale.raw[-12,12])) +
  sum(as.numeric(NcontFemale.raw[-12,11]* NcontFemale.raw[-12,12])) +
  sum(as.numeric(NcontMale.raw[-12,11]  * NcontMale.raw[-12,12]))
)
# The total salary computed from page AV p49 - 50 is 10.708b. It is very close to the value from the imputed table (10.715b),
# but far from the "Appropriation Salary" on page 33.
# This outcome raises the question of how "Appropriation Salary" is defined and why it is much less than the sum of salary.  




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
