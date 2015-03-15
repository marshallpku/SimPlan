library(rcurl)
library(jsonlite)

#1) Get Census Bureau state quarterly tax daata
# get qtax - EVERYTHING from 1992 forward
censusapikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
pre <- "http://api.census.gov/data/eits/qtax?key="
post <- "&get=cell_value,time_slot_name,seasonally_adj,geo_level_code,category_code,data_type_code,error_data"
url <- paste0(pre, censusapikey, post) # note that a censusapikey is needed
# you can paste this url into a browser window: http://api.census.gov/data/eits/qtax?key=b27cb41e46ffe3488af186dd80c64dce66bd5e87&get=cell_value,time_slot_name,seasonally_adj,geo_level_code,category_code,data_type_code,error_data
system.time(qtdat <- fromJSON(url)) # return results as a matrix # about 40-50 secs
df <- data.frame(qtdat)

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