# CalpersExtractforJosh.r
# Don Boyd
# 3/5/2014


# This is an extract from a larger program I wrote in 2013 to see what was involved in using information in actuarial
# valuations in a simulation model




# Get CalPERS assumptions and pop data info and push it around to make it useable in sim model
# Source is the CalPERS 2011 State and School Actuarial Valuation
# For now, just use State Miscellenaous Tier 1 (smt1)

# CAUTION: When creating smoothed functions, inspect inputs and outputs for direction changes that could cause the spline to get off course
# ALTERNATIVE: Consider simple constant-growth interpolation between each point, without taking overall shape into account
# this might be implemented with less intervention

#****************************************************************************************************
#
#                Load any needed packages ####
#
#****************************************************************************************************
library(gdata)
library(reshape2)
library(ggplot2)


#****************************************************************************************************
#
#                Define file locations etc. ####
#
#****************************************************************************************************
cpdir<-paste0(getwd(), '/Data/') 
cpfn<-"CalPERS(9).xlsx"

# Key tables (sheets) in the workbook, for State Miscellenaous Tier 1 (smt1)
# assumptions
salgrow<-"smt1salgrow"
prmort<-"prmortality"
svcretprob<-"svcretirement"

# data
actnumageyos<-"smt1actnumaayos"
actsalageyos<-"smt1actsalaayos"
termnumageyos<-"smt1termnumaayos"
retnumagetype<-"smt1retnumaartype"
retbenagetype<-"smt1retbenaartype"


# yos mapping - we'll need this several times
yoslev<-c("yos0-4","yos5-9","yos10-14","yos15-19","yos20-24","yos25+","yosall")
yoslab<-c(2,7,12,17,22,28,0) # the assumed average yos in each yos group - an assumption I had to make


# check that we can read the relevant sheets
read.xls(paste0(cpdir,cpfn),sheet=svcretprob,colClasses="character")


#****************************************************************************************************
#
#                Tools ####
#
#****************************************************************************************************

cgfill<-function(df,idvar,fillstart){
  # constant-growth fill-in between points in a dataframe
  # return a new data frame which appends, in proper sort order, the filled-in rows
  # function presumes the first column is the id column, a measure of years, and all other columns are numeric, to be filled in
  idx1<-which(df[,idvar]==fillstart)
  measvnums<-2:ncol(df)
  startperiod<-df[idx1,idvar]; startvals<-df[idx1,measvnums]
  endperiod<-df[idx1+1,idvar]; endvals<-df[idx1+1,measvnums]
  nperiods<-endperiod-startperiod-1
  rates<-(endvals/startvals)^(1/(endperiod-startperiod))
  mat<-outer(1:nperiods, 1:length(measvnums), function(i,j) unlist(startvals)[j]*(rates[j]^i)) # returns the interpolated values as a matrix
  newdf<-cbind((startperiod+1):(endperiod-1),as.data.frame(mat))
  names(newdf)<-names(df)
  outdf<-rbind(df,newdf)
  outdf<-outdf[order(outdf[,idvar]),]
  return(outdf)  
}


# create long data frame with spline, from matrix
makelong<-function(df,rowvar,colvar){
  dfl<-melt(df,id=rowvar,variable.name=colvar)
  dfl[,colvar]<-as.numeric(gsub("[^0-9]", "", dfl[,colvar]))
  return(dfl)
}


splong<-function(df,fillvar,fitrange=NULL){
  # df should have only 3 columns: fillvar, nonfillvar [in either order], and value
  # or just 2 columns, with no nonfillvar
  # last column ALWAYS must be the value var
  valvar<-names(df)[length(names(df))]
  nonfillvar<-setdiff(names(df),c(fillvar,valvar))
  f<-function(x) {
    if(is.null(fitrange)) fitrange<-min(x[,fillvar]):max(x[,fillvar])
    spl<-spline(x[,fillvar], x[,valvar], xout=fitrange, method="natural")
    dfout<-data.frame(x=spl$x, y=spl$y)
    names(dfout)<-c(fillvar,valvar)
    return(dfout)
  }
  if(length(nonfillvar)>0) dfl2<-ddply(df,c(nonfillvar),f) else dfl2<-f(df)
  return(dfl2)
}

cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric


#****************************************************************************************************
#
#                Salary growth rates by years of service and entry age ####
#
#****************************************************************************************************
# produce a data frame named salarygrowth
(df<-read.xls(paste0(cpdir,cpfn),sheet=salgrow,colClasses="character"))
str(df)
names(df)<-c("yos",paste0("eage",c(20,30,40)))
df<-df[-1,]
(df<-colwise(cton)(df))
dfl<-makelong(df,"yos","eage")
head(dfl); tail(dfl)
fc<-function(df) {print(count(df,"eage")); print(count(df,"yos"))}
fc(dfl)
# take a quick look
qplot(eage,value,data=dfl,colour=as.factor(yos),geom=c("point","line")) # looks ok
qplot(yos,value,data=dfl,colour=as.factor(eage),geom=c("point","line")) # looks ok

# get splines using one of the idvars
df
dfl2<-splong(dfl,"yos",0:40) # extend the ending point
qplot(yos,value,data=dfl2,colour=as.factor(eage),geom=c("point","line"),ylim=c(0.025,.14)) # looks ok
salarygrowth<-splong(dfl2,"eage",20:60) # get splines the other way, too, to fill out the matrix, but extend the starting and ending points
qplot(eage,value,data=salarygrowth,colour=as.factor(yos),geom=c("point","line"),ylim=c(0,.125)) # some sharply slowing growth above age 55; leave as is for now
# salarygrowth$value<-ifelse(salarygrowth$value<0,0,salarygrowth$value)
# check rates to be sure we match those in the original table
eages<-c(20,30,40); yoss<-unique(df$yos)
check<-subset(salarygrowth,eage %in% eages & yos %in% yoss)
df
dcast(check,yos~eage)
round(df-dcast(check,yos~eage),4)
# good, clean up and we are done
head(salarygrowth)
names(salarygrowth)[3]<-"salarygrowth"
salarygrowth


#****************************************************************************************************
#
#                Service retirement probabilities ####
#
#****************************************************************************************************
# create atorp - active-to-retirement probabilities Note: this produces bad results unless we fill in ages 65-70
(df<-read.xls(paste0(cpdir,cpfn),sheet=svcretprob,colClasses="character"))
names(df)<-c("age",paste0("yos",seq(5,35,5)))
df<-df[-c(1:2),]
(df<-colwise(cton)(df))

dfl<-makelong(df,"age","yos")
head(dfl); tail(dfl)
fc<-function(df) {print(count(df,"age")); print(count(df,"yos"))}
fc(dfl)
# take a quick look
qplot(age,value,data=dfl,colour=as.factor(yos),geom=c("point","line")) # looks like 65-70 will cause trouble
qplot(yos,value,data=dfl,colour=as.factor(age),geom=c("point","line"))
# get splines using one of the idvars
dfl2<-splong(dfl,"age")
qplot(age,value,data=dfl2,colour=as.factor(yos),geom=c("point","line")) # I don't like what happens between ages 65 and 70 because...
# ..the probability goes down, then up, even thought the prob at 70 is greater than the prob at 65

# to fix ages 65 and 70, force-fill the matrix based on compound growth rates so that we travel pretty linearly from 65 to 70
df
(df2<-cgfill(df,"age",65)) # now there is a pretty straight path from 65 to 70 and the spline should not get pushed about
# try again
dfl<-makelong(df2,"age","yos")
head(dfl); tail(dfl)
fc(dfl)
dfl2<-splong(dfl,"age")
fc(dfl2)
qplot(age,value,data=dfl2,colour=as.factor(yos),geom=c("point","line")) # now ages 65 and 70 look reasonable albeit with a kink
dfl3<-splong(dfl2,"yos",0:max(dfl2$yos)) # get splines the other way, too, to fill out the matrix, but start with 0 yos (???)
fc(dfl3)
# we should be all done - look at some graphs
qplot(age,value,data=dfl3,colour=as.factor(yos),geom=c("point","line"))
qplot(yos,value,data=dfl3,colour=as.factor(age),geom=c("point","line"))
# for low yos we have negative retirement rates; set to 0? for now, rerun without going to 0 yos - maybe no one can retire before 5 yos??
dfl3<-splong(dfl2,"yos")
qplot(age,value,data=dfl3,colour=as.factor(yos),geom=c("point","line"))
qplot(yos,value,data=dfl3,colour=as.factor(age),geom=c("point","line"))
# check rates to be sure we match those in the original table
ages<-c(seq(50,62,2),65,70,75); yoss<-seq(5,35,5)
check<-subset(dfl3,age %in% ages & yos %in% yoss)
df
dcast(check,age~yos)
round(df-dcast(check,age~yos),4) # good - ok to save service retirement rates
atorp<-dfl3
names(atorp)[3]<-"atorp" # now suitable for saving
head(atorp); tail(atorp)

