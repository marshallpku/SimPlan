# Importing census data of active members and making impulation
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
data.file <- "TPAF Census Data.xlsx"

options(stringsAsFactors = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                        Functions                                    ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create long data frame with spline, from matrix
makelong<-function(df,rowvar,colvar){
  dfl<-melt(df,id=rowvar,variable.name=colvar)
  dfl[,colvar]<-as.numeric(gsub("[^0-9]", "", dfl[,colvar]))
  return(dfl)
}

splong<-function(df,fillvar,fitrange=NULL, method = "natural"){
  # df should have only 3 columns: fillvar, nonfillvar [in either order], and value
  # or just 2 columns, with no nonfillvar
  # last column ALWAYS must be the value var
  valvar<-names(df)[length(names(df))]
  nonfillvar<-setdiff(names(df),c(fillvar,valvar))
  f<-function(x) {
    if(is.null(fitrange)) fitrange<-min(x[,fillvar]):max(x[,fillvar])
    spl<-spline(x[,fillvar], x[,valvar], xout=fitrange, method = method)
    dfout<-data.frame(x=spl$x, y=spl$y)
    names(dfout)<-c(fillvar,valvar)
    return(dfout)
  }
  if(length(nonfillvar)>0) dfl2<-ddply(df,c(nonfillvar),f) else dfl2<-f(df)
  return(dfl2)
}

cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric

fill_mean <- function(df, wildth, along = c("col", "row")){
  
  fun.expand <- function(x){rep(x/wildth, each = wildth)}
  
  df <- switch(along,
               col = as.data.frame(df),
               row = as.data.frame(t(df))
               )
  
  df <- colwise(fun.expand)(df)
  
  df <- switch(along,
               col = as.matrix(df),
               row = t(as.matrix(df)))
  return(df)
  
}
# 
# fill_mean(df[-1], 5, "col")
# fill_mean(df[-1], 5, "row")
# fill_mean(df[-1], 5, "col") %>% fill_mean(5, "row")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                        Assumptions                                 #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Notes
  # To maintain consistency, all members with entry age < 20 will be excluded.(cells to the northest of the diagnal)


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




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Active Contributory, Male                  #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

df.raw <- read.xls(paste0(data.path, data.file), sheet = "ActContMale", skip = 1, na.strings = "NA", header = TRUE)


# Age/yos distribution
df <- df.raw %>%  select(-age, -Total, - Salary)
df <- df[-c(1, nrow(df)),] 

get_mat <- function(df, col, start, span_age = 5, span_yos = 5){
  # span_age <- 5
  # span_yos <- 5
  # start <- 2
  
  x <- df[[col]]
  x[seq_along(x) < start] <- 0 # exclude cell with entry age < 20
  
  mat <- lapply(x, function(x) x/(span_age * span_yos) * matrix(1, nrow = span_age, ncol = span_yos))
  mat[[start]] <- mat[[start]]*0
  mat[[start]][lower.tri(mat[[start]], diag = TRUE)] <- x[start]/(span_age * (span_yos + 1)/2)
  mat <- rbind.fill.matrix(mat)
  return(mat)
}

df1 <- lapply(1:ncol(df), function(x) get_mat(df, x, x))
df1 <- do.call("cbind", df1) %>% data.frame

colnames(df1) <- 0:44
df1$age <- age_min:69


# df <- fill_mean(df[-1], 5, "col") %>% fill_mean(5, "row") %>% data.frame
# df$age <- age_min:69
# names(df)[-ncol(df)] <- paste0("yos", 0:44)


## Smooth average salary of age groups. 
df.salary <- df.raw %>% select(age, Salary)
df.salary <- df.salary[-c(1, nrow(df.salary)),]
df.salary$age <- seq(22, by = 5, length.out = nrow(df.salary))

# Check the graph
qplot(age, Salary, data = df.salary, geom = c("line","point")) # looks ok.

# Smooth using spline function
df.salary1 <- splong(df.salary, "age", age_min:age_active_max)
qplot(age, Salary, data = df.salary1, geom = c("line","point")) 




## Write the steps above into a function, which can be applied to census tables of other member types. 

get_census_fillmean <- function(df.raw){
  
  # Age/yos distribution
  df <- df.raw %>%  select(-age, -Total, - Salary)
  df <- df[-c(1, nrow(df)),] 
  
  get_mat <- function(df, col, start, span_age = 5, span_yos = 5){
    # span_age <- 5
    # span_yos <- 5
    # start <- 2
    
    x <- df[[col]]
    x[seq_along(x) < start] <- 0 # exclude cell with entry age < 20
    
    mat <- lapply(x, function(x) x/(span_age * span_yos) * matrix(1, nrow = span_age, ncol = span_yos))
    mat[[start]] <- mat[[start]]*0
    mat[[start]][lower.tri(mat[[start]], diag = TRUE)] <- x[start]/(span_age * (span_yos + 1)/2)
    mat <- rbind.fill.matrix(mat)
    return(mat)
  }
  
  df1 <- lapply(1:ncol(df), function(x) get_mat(df, x, x))
  df1 <- do.call("cbind", df1) %>% data.frame
  
  colnames(df1) <- 0:44
  df1$age <- age_min:69
  
  
  ## Smooth average salary of age groups. 
  df.salary <- df.raw %>% select(age, Salary)
  df.salary <- df.salary[-c(1, nrow(df.salary)),]
  df.salary$age <- seq(22, by = 5, length.out = nrow(df.salary))
  
  # Smooth using spline function
  df.salary1 <- splong(df.salary, "age", age_min:age_active_max)
  df.salary1$year <- 2013
  df.salary1 <- select(df.salary1, year, everything())
  
  # results
  output <- list(census = df1, salary = df.salary1)
}

df.raw <- read.xls(paste0(data.path, data.file), sheet = "ActContMale", skip = 1, na.strings = "NA", header = TRUE)
result <- get_census_fillmean(df.raw)

census_ActContMale <- result$census
salary_ActContMale <- result$salary

# Problem: Salary increases to much after 60. It should be relatively flat or even decreasing over time.  


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Active Contributory, Female                #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

df.raw <- read.xls(paste0(data.path, data.file), sheet = "ActContFemale", skip = 1, na.strings = "NA", header = TRUE)
result <- get_census_fillmean(df.raw)

census_ActContFemale <- result$census
salary_ActContFemale <- result$salary



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Active Non-Contributory, Male                #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

df.raw <- read.xls(paste0(data.path, data.file), sheet = "ActNcontMale", skip = 1, na.strings = "NA", header = TRUE)
result <- get_census_fillmean(df.raw)

census_ActNcontMale <- result$census
salary_ActNcontMale <- result$salary

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Active Non-Contributory, Female                #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

df.raw <- read.xls(paste0(data.path, data.file), sheet = "ActNcontFemale", skip = 1, na.strings = "NA", header = TRUE)
result <- get_census_fillmean(df.raw)

census_ActNcontFemale <- result$census
salary_ActNcontFemale <- result$salary



#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Saving Results                                 #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save(census_ActContMale, census_ActContFemale,
     census_ActNcontMale, census_ActNcontFemale,
     file = paste0(data.path,"census_active.RData"))

save(salary_ActContMale, salary_ActContFemale, 
     salary_ActNcontMale, salary_ActNcontFemale, 
     file = paste0(data.path,"salary13_active.RData"))










#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                         Experiment with Smoothing methods          #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(df.raw <- read.xls(paste0(data.path, data.file), sheet = "ActContMale", skip = 1, na.strings = "NA", header = TRUE))

# Convert the age/yos range to the middle age/yos of the group. 
# Age group "65 and up" is converted to 67; 
mid_age <- seq(22, by = 5, length.out = 10)



## Smooth average salary of age groups. 
df.salary <- df.raw %>% select(age, Salary)
df.salary <- df.salary[-c(1, nrow(df.salary)),]
df.salary$age <- seq(22, by = 5, length.out = nrow(df.salary))

 # Check the graph
qplot(age, Salary, data = df.salary, geom = c("line","point")) # looks ok.

 # Smooth using spline function
df.salary1 <- splong(df.salary, "age", age_min:age_active_max)
qplot(age, Salary, data = df.salary1, geom = c("line","point")) 
# Problem: Salary increases to much after 60. It should be relatively flat or even decreasing over time.  


## Smooth age/yos distribution

 # Notes
  # Smoothing results after age 60 and yos 40 can be very unsatisfactory, for now we only extrapolate age up to 64 and yos up to 44. 
  #

df <- df.raw %>%  select(-Total, - Salary)
df <- df[-c(1, nrow(df)),] 
df$age <- seq(22, by = 5, length.out = nrow(df))
names(df)[-1] <- seq(2, by =  5, length.out = ncol(df) - 1)
df <- gather(df, yos, value, -age) %>% 
  mutate(value = value/25,
         yos = as.numeric(levels(yos)[yos]) ) # assume number of members at group middle age is 1/5 of the group total.


 # Check the graph of population by yos
qplot(age, value, data = df, colour = as.factor(yos), geom = c("line","point"))


 # Smooth by using spline: order: 1. age; 2. yos
df1_age <- splong(df, "age", age_min:64, method = "natural")
qplot(age, value, data = df1_age, colour = as.factor(yos), geom = c("line","point")) # some values are negative
qplot(yos, value, data = df1_age, colour = as.factor(age), geom = c("line","point"))

df2_age_yos <- splong(df1_age, "yos", 0:44, method = "natural")
qplot(age, value, data = df2_age_yos, colour = as.factor(yos), geom = c("line","point")) 


df2_age_yos %<>% mutate(value = round(value),
                        value = ifelse(value >=0, value, 0)) 

df2 <- spread(df2_age_yos, yos, value)
df2

# Check against original data. 
sum(df)*25
sum(df2)
colSums(df2)
rowSums(df2)



