## Examine variables PPD for constructing prototypical plans and funing scenaniros. 
## Yimeng Yin
## 4/19/2015


#**************************************************************************
#                            Preamble                                 #####
#**************************************************************************

library(knitr)
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(jsonlite)
library(xlsx)

# Create a function to make query into PPD.
PPDQuery <- function(queryType, variables = NULL, dataset = NULL, filterfy = NULL, format = "csv", filterppdid = NULL, filtereegroupid = NULL, filtertierid = NULL){
  
  queryType <- paste0("?q=", queryType)
  if(!is.null(variables)) variables <- paste0("&variables=",paste0(variables, collapse = ","))
  if(!is.null(filterfy)) filterfy <- paste0("&filterfystart=",filterfy[1],"&filterfyend=",filterfy[2]) 
  if(!is.null(filterppdid)) filterppdid <- paste0("&filterppdid=", paste0(filterppdid, collapse = ","))
  if(!is.null(filtereegroupid)) filterppdid <- paste0("&filtereegroupid=", paste0(filtereegroupid, collapse = ","))
  if(!is.null(filtertierid)) filterppdid <- paste0("&filtertierid=", paste0(filtertierid, collapse = ","))
  format <- paste0("&format=", format)
  
  queryURL <- paste0(PPDapi, queryType, variables, filterfy, filterppdid, filtereegroupid, filtertierid, format)
}

# Test the function
PPDQuery(queryType = "QVariables", variables = c("fy", "PlanName", "InvestmentReturn_5yr"),
         filterfy = c(2009, 2011), filterppdid = c(12, 13, 14)) %>% read.csv


#**************************************************************************
#                            Variables                                 #####
#**************************************************************************


PPD <- read.xlsx2("Data/PPD_1_7_2015.xlsx", sheetIndex = 1)

PPD.var <- PPD %>% select(ppd_id, PlanName, fy,
# Funded status
ActFundedRatio_GASB, # GASB funded ratio
PercentReqContPaid,  # Percent of ARC paid

# Membership
ActiveAge_avg,
ActiveTenure_avg,
ServiceRetireeAge_avg,

actives_tot,
beneficiaries_ServiceRetirees,

# Benefit structure
# COLABasis, # COLA
# NormRetEligibility, # Normal retirement eligibility
# vesting,            # vesting for new hires
# NormRetBenefit,     # benefit factor
                      # final average salary ?


# Scenario-contribution determination
ActCostMethCode_GASB,  # GASB actuarial method
AssetSmoothingPeriod_GASB, # GASB asset smoothing periods 
InvestmentReturnAssumption_GASB, # GASB expected investment return (discount rate)

FundingMethCode1_GASB,   # Amort payment type 1) CP; 2) CD
FundingMethCode2_GASB,   # Amort period type  1) fixed; 2) closed  ? how does the closed period amort work in practice? 
UAALAmortPeriod_GASB    # Amort period        
) %>% 
filter(fy == 2013)

f2n <- function(x) as.numeric(levels(x)[x])


# Funded status
f2n(PPD.var$ActFundedRatio_GASB) %>% hist(main = "funded ratio in 2013")
f2n(PPD.var$PercentReqContPaid) %>% hist(main = "Percent of ARC paid in 2013")


# membership
f2n(PPD.var$ActiveAge_avg) %>% hist(main = "Average age of Actives in 2013", breaks = 20)
f2n(PPD.var$ActiveTenure_avg) %>% hist(main = "Average yos of Actives in 2013", breaks = 20)
f2n(PPD.var$ServiceRetireeAge_avg) %>% hist(main = "Average age of retirees in 2013", breaks = 20)

f2n(PPD.var$actives_tot) %>% hist(main = "Number of Actives in 2013", breaks = 20)
f2n(PPD.var$beneficiaries_ServiceRetirees) %>% hist(main = "Number of Retirees in 2013", breaks = 20)

(f2n(PPD.var$beneficiaries_ServiceRetirees) / f2n(PPD.var$actives_tot))[-55] %>% hist(main = "Retirees vs Actives in 2013", breaks = 20)


# Funding 

f2n(PPD.var$ActCostMethCode_GASB) %>% hist(main = "Actuarial Method", breaks = 4)

f2n(PPD.var$ActCostMethCode_GASB) %>% table  # *EAN (1), PUC (2), FIL (3), AGG (4), OTHER (5)
f2n(PPD.var$InvestmentReturnAssumption_GASB) %>% hist(main = "Assumed Investment return", breaks = 10)

f2n(PPD.var$UAALAmortPeriod_GASB)  %>% hist(main = "Amortization period", breaks = 10)
f2n(PPD.var$FundingMethCode1_GASB) %>% hist(main = "Amortization payment type: CP(1); CD(0)", breaks = 2)
f2n(PPD.var$FundingMethCode2_GASB) %>% hist(main = "Amortization period type: Open(1) Fixed(2) Closed(3)", breaks = 6)


f2n(PPD.var$FundingMethCode1_GASB) %>% table  # Amortization payment type:CD(0), *CP(1) 
f2n(PPD.var$FundingMethCode2_GASB) %>% table  # Amortization period type: Open(1) Fixed(2) *Closed(3)
 







