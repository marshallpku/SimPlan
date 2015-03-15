

# http://publicplansdata.org/public-plans-database/api/  for documentation

library(jsonlite)

# get a list of datasets
fromJSON("http://publicplansdata.org/api/?q=ListDataSets&format=json") %>% # too bad this doesn't return more info about the ds
  select(id, table_name, table_display_name) %>% filter(row_number()>1)

# get list of variables within datasets
url <- "http://publicplansdata.org/api/?q=ListVariables&format=json" # CRR example shows format=xml but that does not work
dsvars <- fromJSON(url) %>% 
  select(-status, -date, -q, - params, - recordcount) %>% 
  filter(row_number()>1) %>%
  arrange(dataset_id, dataset_name, FieldName)
glimpse(dsvars)

# look for interesting variables and the datasets they're in
vpart <- "PercentReqContPaid" # PercentReqContPaid InvestmentReturn
dsvars %>% select(dataset_id, dataset_name, FieldName) %>%
  filter(grepl(vpart, FieldName, ignore.case=TRUE))

# grab a few variables from multiple datasets, without having to specify which ds
api <- "http://publicplansdata.org/api/?"
qtype.qv <- "q=QVariables&variables="
vars <- c("fy","PlanName", "ppd_PercentReqContPaid", "InvestmentReturn_5yr") # needs 2 datasets: nationaldatappd and pensioninvestmentreturn
vstring <- paste0(vars, collapse=",")
filter <- "&filterfystart=2009&filterfyend=2011"
fmt <- "&format=json"
url <- paste0(api, qtype.qv, vstring, filter, fmt)
df <- fromJSON(url)
glimpse(df)
# this is confusing - it returns ppd_PercentReqContPaid for each plan and year, but when we look for the var we only find it in
# nationaldatappd; yet when I download that entire dataset (below), it appears to have national data only, not plan-specific data
df %>% select(fy, PlanName, ppd_PercentReqContPaid) %>%
  spread(fy, ppd_PercentReqContPaid)
# the answer is that the query pastes the NATIONAL ppd_PercentReqContPaid onto every plan

# get an entire dataset of choice
api <- "http://publicplansdata.org/api/?"
qtype.ds <- "q=QDataSet&dataset="
# here are a few of the interesting ones; I cannot find the full ppd, however
# ppdstatedata nationaldatappd pensionsystemdata pensionincomestatement nationaldatacensus pensionplanbasics
# pensionfundingandmethods pensiongasbschedules
dsn <- "pensiongasbschedules" 
fmt <- "&format=csv"
csv <- paste0(api, qtype.ds, dsn, fmt)
df.full <- read.csv(csv) # this is convenient but let's not trust it fully until we've made sure no info lost relative to JSON, since it converts character to numeric
glimpse(df.full)

# read the full ppd directly from the web
# here's the url found at http://publicplansdata.org/public-plans-database/download-full-data-set/
url <- "http://publicplansdata.org/wp-content/uploads/2015/01/PPD_1_7_2015.xlsx" 
df.ppd <- read.xls(url, colClasses="character") # ALWAYS safest to read data as character and convert to numeric AFTER inspecting it
glimpse(df.ppd)

# good, this has PercentReqContPaid; where does it come from - looks like pensiongasbschedules
count(df.ppd, fy)
df.ppd %>% mutate(PercentReqContPaid=cton(PercentReqContPaid)) %>%
  group_by(fy) %>%
  summarise(nnum=sum(!is.na(PercentReqContPaid)))

# in case you don't have this:
qtiledf < - function (vec, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)) as.data.frame(t(quantile(vec, na.rm = TRUE, probs)))

df.ppd %>% mutate(PercentReqContPaid=cton(PercentReqContPaid)) %>%
  group_by(fy) %>%
  do(qtiledf(.$PercentReqContPaid)) # possibly some bad numbers in here, could reflect POBs