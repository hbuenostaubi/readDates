

# options(repos="https://CRAN.R-project.org")  ### override firewall
#  install.packages("stringr", dependencies=T);install.packages("readxl", dependencies=T);
# install.packages("lubridate", dependencies=T);install.packages("writexl", dependencies=T);

install.packages("reticulate")
library(readxl);library(stringr);library(lubridate);library(writexl);
library(reticulate)


##################################   APV upload
#################################################################################################

setwd("S:/ProdDev/123data/PLANNING/HBueno/Data Downloads")  #Download work directory and uploading docs
xl=read_xlsx("APV.xlsx")

options(warn=-1)
xl=subset(xl, xl$Price!=99999)


####################### Functions
look.bf<-function(x){var=strsplit(x,"-");  #looks before "-"
    paste(var[[1]][1])}
look.af<-function(x){var=strsplit(x,"-");  #looks after "-"
    paste(var[[1]][2])}


# creating the linked list of months
months<-c("1", "2","3", "4", "5", "6","7", "8", "9", "10", "11", "12")
for(i in 1:length(months)){
  months[i]<-month.abb[i]}

month2abb<-function(x){
  if(str_detect(x, "-")==T){
    var1=look.bf(x);var2=look.af(x);
    var1=as.Date(str_trim(var1),"%m/%d/%y");
    var2=as.Date(str_trim(var2),"%m/%d/%y");
    if(month(var1)==month(var2)){paste(months[month(var1)],day(var1), "-", day(var2))}else{
           paste(months[month(var1)],day(var1), "-",months[month(var2)],day(var2))}
  }else{
  var<-as.Date(str_trim(x), "%m/%d/%y")  # turn to date month abb and number
  paste(months[month(var)], day(var))
  }
}

month2abb2<-function(x){
  var<-as.Date(str_trim(x), "%m/%d/%y")   # turn to date number only
  paste(day(var))
}

month2abb3<-function(x){
  var<-as.Date(str_trim(x), "%m/%d/%y")  # turn to date month abb only
  paste(months[month(var)])
}

trim.trailing<-function(x){
  str_replace_all(x, " , ", ", ")
}

##############
#######################  MAIN

# creating a list of lists w/ dates split by comma
# xl$DepartureDate[[1]][2]   <--iterating through list of lists  #print(xl$DepartureDate[[i]][j])
xl$DepartureDate=as.list(strsplit(xl$DepartureDate, ","))



# creating a list of lists w/ dates split by comma
xl$DepartureDate_rev<-NA;for(i in 1:length(xl$DepartureDate_rev)){for(j in 1:length(xl$DepartureDate[[i]])){

  if(is.na(xl$DepartureDate_rev[i])==T){xl$DepartureDate_rev[i]=paste(str_trim(month2abb(xl$DepartureDate[[i]][j])))}

  else if(str_detect(xl$DepartureDate_rev[i], month2abb3(xl$DepartureDate[[i]][j]))==F){xl$DepartureDate_rev[i]=paste(str_trim(xl$DepartureDate_rev[i]),str_trim(", "),str_trim(month2abb(xl$DepartureDate[[i]][j]))) }  #second month

  else if(sum(str_count(month2abb3(xl$DepartureDate[[i]]),month2abb3(xl$DepartureDate[[i]][j])))>1){    ###if dates are not continuous
    xl$DepartureDate_rev[i]=paste(str_trim(xl$DepartureDate_rev[i]),str_trim(", "), str_trim(month2abb2(xl$DepartureDate[[i]][j])))}

  # else if(sum(str_count(month2abb3(xl$DepartureDate[[i]]),month2abb3(xl$DepartureDate[[i]][j])))>1 &
  #         as.numeric(month2abb2(xl$DepartureDate[[i]][j-1]))+1==as.numeric(month2abb2(xl$DepartureDate[[i]][j]))){
  #   xl$DepartureDate_rev[i]=paste(xl$DepartureDate_rev[i],"-", month2abb2(xl$DepartureDate[[i]][j]))
  # }
}};xl$DepartureDate_rev<-trim.trailing(xl$DepartureDate_rev)

xl$DepartureDate<-xl$DepartureDate_rev;xl$DepartureDate_rev<-NULL;



options(warn=0)


write_xlsx(xl, "APV_rev.xlsx",col_names = T)
