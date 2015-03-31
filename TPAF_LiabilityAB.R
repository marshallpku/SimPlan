# Calculate service retirement AL for class A/B
# 3/21/2015
# Yimeng Yin


## Preamble ##################################################################################################
rm(list = ls())

library(knitr)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(microbenchmark)
#library(xlsx)

data.path <- "Data" 
data.file <- "TPAF ES2012 Assumptions.xlsx"

options(stringsAsFactors = FALSE)

source("E:/GitHub/PenSim-Projects/Learn-PenSim/Functions.R")


## Notes #####################################################################################################

# As the first attemp to replicate AL in TPAF AV, some assumptions are made to simply the calculation, or to 
# avoid problems caused by missing information. We may eventually drop these simplying assumptions in latter versions of the program. 
 # 1. Only retirement annuity can be chosen upon early retirement, refund of member contribution is not allowed for now.
 #    The probability of choosing refund upon early retirement is not given in exp study or AV. We will keep this assumption until
 #    we know how the decision of choosing between annuity and refund are made. (A possible solution: take the max of PV of annuity and refund)


age_min <- 20
age_max <- 110
age_active_max <- 80 # Because mortality for active is given only up to age 80

r_min <- 40 # Need to find a more reasonable assumption
r_max <- 81 # Because mortality for active is given only up to age 80

age_full <- 55 # age eligible for full retirement benefit
yos_full <- 25 # yos eligible for full retirement benefit
age_sr   <- 60 # age eligible for service retirement (full benefit without yos requirement)

ea_min <- age_min # Because termination and disability rates start with 25
ea_max <- 59

yos_max <- r_max - ea_min # max possible yos

i <- 0.079
v <- 1/(1 + i)
fasyears  <- 3
benfactor <- 1/55

## Load results needed in the calculation ####################################################################
load(paste0(data.path, "/salaryTable_active.RData"))
load(paste0(data.path, "/decrement_mort.RData"))
load(paste0(data.path, "/decrement_disb.RData"))
load(paste0(data.path, "/decrement_term.RData"))
load(paste0(data.path, "/decrement_retire.RData"))
load(paste0(data.path, "/census_active.RData"))

## Create a data frame that contain all possible combinations of start year, ea, age and yos 

 # The maximum year needed: 2013 + 61 = 2074, a 2013 entrant at age 20 will reach the max yos=61 (20 ~ 80) in 2074.
 # The minimum year needed: 2013 - 61 + 1 = 1953, a 1953 entrant at age 20 will reach the max yos=61 (20 ~ 80) in 2013.
 # The maximum yos needed:  61

# input data

SS        <- SS_ActConMale
census    <- census_ActContMale

mort        <- mortMale
disb        <- disbMale
term_AB_rf  <- termMale_AB_rf
term_AB_vest<- termMale_AB_vest
retire_AB   <- retireMale_AB

get_AL_PUC_AB <- function(SS, census,
                          mort, 
                          disb,        
                          term_AB_rf,  
                          term_AB_vest,
                          retire_AB){

#   SS        <- SS_ActConMale
#   census    <- census_ActContMale
#   
#   mort        <- mortMale
#   disb        <- disbMale
#   term_AB_rf  <- termMale_AB_rf
#   term_AB_vest<- termMale_AB_vest
#   retire_AB   <- retireMale_AB  
#   
  
AL_retireAB <- 
  expand.grid(start.year = 1953:2013, ea = age_min:age_active_max, age = age_min:age_max) %>%
  mutate(yos  = age - ea,
         year = start.year + yos,
         include = start.year + age_active_max - ea >= 2013 # only include those whose career period include 2013
  ) %>%
  filter(age >= ea, include == TRUE) %>%
  arrange(start.year, ea, age) %>%
  select(-include) 

#### Try Contributory Female first 

## Merge salary table and decrement tables 

AL_retireAB %<>% 
  left_join(SS %>% select(-growth, -scale, -scale13, -Salary)) %>%
  left_join(mort) %>% 
  left_join(disb) %>% 
  left_join(term_AB_rf) %>% 
  left_join(term_AB_vest) %>% 
  left_join(retire_AB)

na2zero <- function(x){x[is.na(x)] <- 0 ;return(x)}
AL_retireAB<- colwise(na2zero)(AL_retireAB)

AL_retireAB %<>%
  ungroup %>% # for safety
  group_by(start.year, ea) %>% 
  # Reduction rate 
  mutate(gx.r = ifelse(age >= 60, 1, 0),
         gx.r = ifelse(age >=55 & yos >= 25, 1, gx.r),
         gx.r = ifelse(age < 55 & yos >= 25, 1 - (55 - age) * 0.03, gx.r)) %>% 
  # Probability of Survival
  mutate(pxm_act_o  = 1 - qxm_act_o,  # active member surviving ordinary death
         pxm_post_h = 1 - qxm_post_h, # healthy service retiree surviving death
         pxm_post_d = 1 - qxm_post_d, # disabled retiree surviving death
         pxT_act    = 1 - qxm_act_o - qxd_o - qxd_a - qxt.rf - qxt.vest - qxr,
         pxT_post_h = pxm_post_h
  )


AL_retireAB %<>%
   filter(ea < 80) %>% 
  # Benefits
  mutate(Sx = ifelse(age == min(age), 0, lag(cumsum(sx))), # Cumulative salary
         n  = pmin(yos, fasyears),                         # years used to compute fas
         fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
         fas= ifelse(age == ea, 0, fas),
         Bx = benfactor * yos * fas,                  # accured full retirement benefit. multiplied by gx.r to get actual benefit
         bx = lead(Bx) - Bx, 
         ax_h = get_tla(pxm_post_h, i),                                     # Since retiree die at 110 for sure, the life annuity is equivalent to temporary annuity up to age 110. Mortality only
         ax80 = c(get_tla(pxT_act[age<=80], i), rep(0, 30)),                # PV of of annuity from age x to 80 at age x
         ax80s= c(get_tla(pxT_act[age<=80], i, sx[age<=80]), rep(0, 30)),
         ayx = c(get_tla2(pxT_act[age<=81], i), rep(0, 29)),
         ayxs= c(get_tla2(pxT_act[age<=81], i, sx[age<=81]), rep(0,29))
        )
                      

# Calculate NC and AL
tab_AL_retireAB <- AL_retireAB %>%  
  mutate(TCx.r = gx.r * Bx * qxr * ax_h,  # term cost of retirement
         PVFBx.r = c(get_PVFB(pxT_act[age <= 81], v, TCx.r[age <= 81]), rep(0, 29)),
         # NC and AL of PUC
         TCx.r1 = gx.r * qxr * ax_h,  # term cost of $1's benefit
         NCx.PUC = bx * c(get_NC.PUC(pxT_act[age <= 81], v, TCx.r1[age <= 81]), rep(0, 29)),
         ALx.PUC = Bx * c(get_PVFB(pxT_act[age <= 81], v, TCx.r1[age <= 81]), rep(0, 29)),
        
         # NC and AL of EAN.CD
         NCx.EAN.CD = ifelse(age < 81, PVFBx.r[age == min(age)]/ayx[age == 81], 0),
         ALx.EAN.CD = PVFBx.r - NCx.EAN.CD * ax80,
         # NC and AL of EAN.CP
         NCx.EAN.CP = ifelse(age < 81, sx * PVFBx.r[age == min(age)]/(sx[age == min(age)] * ayxs[age == 81]), 0),
         ALx.EAN.CP = PVFBx.r - NCx.EAN.CP * ax80s
  )
               

# Extract information for calculating total AL and NC in 2013

AL.PUC_retireAB13 <- tab_AL_retireAB %>% 
  ungroup %>% 
  filter(year == 2013) %>% 
  select(age, yos, ALx.PUC) %>% 
  spread(yos, ALx.PUC)


# Keep class AB in the census table (hired before 2007 so yos > 6 in 2013)
census_AB <- census %>% select(-age)
census_AB[,1:7] <- 0

# Taylor the AL table to fit the dimension of census table
AL.PUC_retireAB13 %<>% filter(age <=69) %>% select(one_of(as.character(0:44)))


# Calculate the total AL

tot_AL.PUC.AB <- (na2zero(census_AB)*na2zero(AL.PUC_retireAB13)) %>% as.matrix %>% sum
return(tot_AL.PUC.AB)
}


AL.ConFemale <- get_AL_PUC_AB(SS_ActConFemale, census_ActContFemale,
              mortFemale, disbFemale, termFemale_AB_rf, termFemale_AB_vest, retireFemale_AB)

AL.ConMale <- get_AL_PUC_AB(SS_ActConMale, census_ActContMale,
              mortMale, disbMale, termMale_AB_rf, termMale_AB_vest, retireMale_AB)

AL.NconFemale <- get_AL_PUC_AB(SS_ActNconFemale, census_ActNcontFemale,
              mortFemale, disbFemale, termFemale_AB_rf, termFemale_AB_vest, retireFemale_AB)

AL.NconMale <- get_AL_PUC_AB(SS_ActNconMale, census_ActNcontMale,
              mortMale, disbMale, termMale_AB_rf, termMale_AB_vest, retireMale_AB)

AL.tot <- AL.ConFemale + AL.ConMale + AL.NconFemale + AL.NconMale

AL.ConFemale
AL.ConMale
AL.NconFemale
AL.NconMale
AL.tot




## Target:
 # Total Service Retirement AL: 17347343048 (17.3b)

# Class A/B service retirement AL
# 9701205544 + # contributory Female
# 3099007112 + # contributory Male
# 103369070 +  # noContributory Male 
# 343079767    # noContributory Female
# Total:13246661493  (13.2b)


# 3/31 Imputing salary across yos
10484627415 +  # Con Female
3438982024  +  # Con Male
235893774   +  # Ncon Female
72289947       # Ncon Male   
# Total:14231793160  (14.2b)
# ALs of contributory members increase while ALs of noncontributory members decrease  
# Total AL increases by about 1 billion. 


