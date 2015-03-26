# This program import and and explore PPD data.
# 3/24/2015
# Yimeng Yin


## Preamble ##################################################################################################

library(RCurl)
library(jsonlite)
library(magrittr)


## Tools #####################################################################################################

# Function to generate a query URL
PPDQuery <- function(queryType, variables = NULL, dataset = NULL, filterfy = NULL, format = "csv", filterppdid = NULL, filtereegroupid = NULL, filtertierid = NULL){
  
  PPDapi <- "http://publicplansdata.org/api/"
  
  queryType <- paste0("?q=", queryType)
  if(!is.null(variables)) variables <- paste0("&variables=",paste0(variables, collapse = ","))
  if(!is.null(filterfy)) filterfy <- paste0("&filterfystart=",filterfy[1],"&filterfyend=",filterfy[2]) 
  if(!is.null(filterppdid)) filterppdid <- paste0("&filterppdid=", paste0(filterppdid, collapse = ","))
  if(!is.null(filtereegroupid)) filterppdid <- paste0("&filtereegroupid=", paste0(filtereegroupid, collapse = ","))
  if(!is.null(filtertierid)) filterppdid <- paste0("&filtertierid=", paste0(filtertierid, collapse = ","))
  format <- paste0("&format=", format)
  
  queryURL <- paste0(PPDapi, queryType, variables, filterfy, filterppdid, filtereegroupid, filtertierid, format)
}



## Importing PPD data #######################################################################################

## Examples of importing PPD data from official websites

# 1. ListVariables query: give a list of variables in all PPD datasets. 
ListVariables <- fromJSON("http://publicplansdata.org/api/?q=ListVariables&format=json")

# 2. ListDatasets queryL give a list of all PPD datasets
ListDatasets <- fromJSON("http://publicplansdata.org/api/?q=ListDataSets&format=json")

# 3. QVariables query
QvariablesJSON <- fromJSON("http://publicplansdata.org/api/?q=QVariables&variables=fy,PlanName,InvestmentReturn_5yr&filterfystart=2009&filterfyend=2011&format=JSON")
QvariablesCSV <- read.csv("http://publicplansdata.org/api/?q=QVariables&variables=fy,PlanName,InvestmentReturn_5yr&filterfystart=2009&filterfyend=2011")

# 4. QDataset query
Qdataset <- fromJSON("http://publicplansdata.org/api/?q=QDataset&dataset=pensionplanbasics&format=JSON&filterppdid=12,13,14")


## Importing with PPDQuery function
PPDdat <- PPDQuery(queryType = "QVariables", variables = c("fy", "PlanName", "InvestmentReturn_5yr"),
         filterfy = c(2009, 2011), filterppdid = c(12, 13, 14)) %>% read.csv



## Importing variables for constructing prototype data. ######################################################


# Mentalist of variables
  # Funded status, AA/AL
  # Gov shirking; ERC/ARC; data: PPD, Loop data
  # Risk Taking (Equity/AA)
  # Plan Maturity: AL of retiree/AL Total; median age of actives
  # Plan generosity
  # Growth of actives

variableList <- c("fy", "PlanName", "ppd_id",
                  # Funding Status
                  "MktAssets_ActRpt", # Market value of assets
                  "ActAssets_AVA",    # actuarial value of assets
                  "ActLiabilities_EAN", # AL under EAN
                  "ActLiabilities_PUC", # AL under PUC
                  
                  # Gov shirking
                  "ReqContAmount_ER",  # employer's actuarial required contribution amount
                  "ReqContAmount_tot", # total projected actuarial required contribution amount
                  
                  # Risk taking
                  "equities_tot", # % of assets invested in equities
                  
                  # Plan generosity
                  "COLABasisCode", 
                  "EEContribRate",  # Employee contribution rate for new hire
                  "NormRetEligibility",
                  "EarlyRetEligibility", 
                  "DeferredRetBenefit", 
                  
                  # Plan Maturity /  growth of actives
                  "actives_tot",    # total actives
                  "ActiveAge_avg",  # average age of actives
                  "Beneficiaries_ServiceRetirees", # total number of service retirees
                  "ServiceRetireeAge_avg" # average age of service retirees
                  )


PPDselect <- PPDQuery(queryType = "QVariables", variables = variableList,
                   filterfy =NULL, filterppdid = NULL) %>% read.csv



