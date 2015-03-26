library(RCurl)
library(jsonlite)
library(magrittr)
library(RODBC)

## JSONLite Basics ####
all.equal(mtcars,toJSON(mtcars) %>% fromJSON)
toJSON(c(1:3))
toJSON(data.frame(a=1:3, b=3:5))
toJSON(matrix(1:9, 3))




#1) Get Census Bureau state quarterly tax daata
# get qtax - EVERYTHING from 1992 forward
censusapikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
pre <- "http://api.census.gov/data/eits/qtax?key="
post <- "&get=cell_value,time_slot_name,seasonally_adj,geo_level_code,category_code,data_type_code,error_data"
url <- paste0(pre, censusapikey, post) # note that a censusapikey is needed
# you can paste this url into a browser window: http://api.census.gov/data/eits/qtax?key=b27cb41e46ffe3488af186dd80c64dce66bd5e87&get=cell_value,time_slot_name,seasonally_adj,geo_level_code,category_code,data_type_code,error_data
system.time(qtdat <- fromJSON(url)) # return results as a matrix # about 40-50 secs
df <- data.frame(qtdat[-1,]); colnames(df) <- qtdat[1,]



#2) Get longitude / latitude for an address (no api key)
address <- "11 Kenyon Hill Road, Cambridge, NY, 12816"
urlpre <- "http://www.datasciencetoolkit.org/maps/api/geocode/json?sensor=false&address="
adr2 <- gsub(" ", "+", address)
url <- paste0(urlpre, adr2)
fromJSON(url)


#3) Get JP's data - no api key
pre <- "http://publicplansdata.org/api/?q=QVariables&variables="
vars <- "fy,PlanName,InvestmentReturn_5yr"
filter <- "&filterfystart=2009&filterfyend=2011"
fmt <- "&format=json"
url <- paste0(pre, vars, filter, fmt)
result <- fromJSON(url)
str(result) # returned as a data frame

toJSON(result)
#PPDapi <- "http://publicplansdata.org/api/"

# Examples

# 1. ListVariables query: give a list of variables in all PPD datasets. 
ListVariables <- fromJSON("http://publicplansdata.org/api/?q=ListVariables&format=json")

# 2. ListDatasets queryL give a list of all PPD datasets
ListDatasets <- fromJSON("http://publicplansdata.org/api/?q=ListDataSets&format=json")

# 3. QVariables query
QvariablesJSON <- fromJSON("http://publicplansdata.org/api/?q=QVariables&variables=fy,PlanName,InvestmentReturn_5yr&filterfystart=2009&filterfyend=2011&format=JSON")
QvariablesCSV <- read.csv("http://publicplansdata.org/api/?q=QVariables&variables=fy,PlanName,InvestmentReturn_5yr&filterfystart=2009&filterfyend=2011")

# 4. QDataset query
Qdataset <- fromJSON("http://publicplansdata.org/api/?q=QDataset&dataset=pensionplanbasics&format=JSON&filterppdid=12,13,14")


# Write a function to generate a query URL

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

PPDQuery(queryType = "QVariables", variables = c("fy", "PlanName", "InvestmentReturn_5yr"),
         filterfy = c(2009, 2011), filterppdid = c(12, 13, 14)) %>% read.csv



#4) Open NASRA dataset (and RODBC basics)

# NASRA data is stored in a .mdb file, which needs to be connected using RODBC

channel <- odbcConnectAccess(paste0(getwd(),"/Data/Fund2k.mdb")) 
channel 




