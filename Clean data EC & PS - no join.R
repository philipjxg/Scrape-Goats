library("readr")
library("purrr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("plyr")




##########################################################################################
##### Political Science ##################################################################
##########################################################################################

raw.data.PS=read.csv("C:\\Users\\PhilipJunXian\\OneDrive\\Polit\\Master\\Social Data Science\\Opgave\\staku.csv")
#########################################################
### Clean the data for NA og hyperlink

data.PS = raw.data.PS %>% 
  filter( !is.na(o_kar)==TRUE)  %>% 
  select(-X, -id, -o_gns, -r_gns, -hyperlink)

#########################################################
### clean Ej mødt, Ikke bestået, Syg
### create aggregated grade variable


data.PS$s_kar = ifelse(is.na(data.PS$r_kar)==TRUE,data.PS$o_kar, data.PS$o_kar + data.PS$r_kar )
data.PS$kar.m = as.numeric(levels(data.PS$kar))[data.PS$kar]

data.PS=data.PS %>%  
  filter(!is.na(kar.m)==TRUE)
data.PS$kar.m = NULL

#########################################################
### create semester and year column
data.PS = data.PS %>% 
  separate(sem, c("semester", "year"), 7)

data.PS = as.data.frame(lapply(data.PS,function(x) 
  if(is.character(x)|is.factor(x)) gsub("Winter-","Fall",x) else x))

data.PS = as.data.frame(lapply(data.PS,function(x) 
  if(is.character(x)|is.factor(x)) gsub("Summer-","Spring",x) else x))

data.PS$semester = ifelse(is.na(data.PS$blok)==TRUE, as.character(data.PS$semester), "Summer" )

data.PS$blok = NULL


#########################################################
### correct grades and numeric variables

as.numeric(data.PS$o_kar)
as.numeric(data.PS$r_kar)
as.numeric(data.PS$s_kar)

revalue(data.PS$kar, c("00" = "0", "02" = "2", "-3" = "-3")) -> data.PS$kar 

data.PS$kar = data.PS$kar %>% 
  as.character() %>% 
  as.numeric()



#########################################################
#########################################################
#### preparing data frame for plotting graph

# select variables
data.PS.graf = data.PS %>% 
  select(year, semester, name, kar, s_kar)
data.PS.graf$product = data.PS.graf$s_kar * data.PS.graf$kar

data.PS.graf1 = aggregate(data.PS.graf$product, by=list(data.PS.graf$year, data.PS.graf$semester), FUN=sum, na.rm=TRUE)
colnames(data.PS.graf1)[3] = "sum.product" 

data.PS.graf2 = aggregate(data.PS.graf$s_kar, by=list(data.PS.graf$year,data.PS.graf$semester), FUN=sum, na.rm=TRUE)
colnames(data.PS.graf2)[3] = "count" 

data.PS.graf.plot = left_join(data.PS.graf1, data.PS.graf2)
colnames(data.PS.graf.plot)[1] = "year" 
colnames(data.PS.graf.plot)[2] = "semester" 
data.PS.graf.plot$average = data.PS.graf.plot$sum.product / data.PS.graf.plot$count 

### HUSK - fjern summer: summer 2014  og 2015 5 - Ikke retvisende: 1 og hhv. 5 fag

# data.PS.graf.plot1 = data.PS.graf.plot[!(data.PS.graf.plot$semester=="Summer" ),]


##### Political Science SLUT #############################################################









##########################################################################################
##### Economics ##########################################################################
##########################################################################################


raw.data.EC=read.csv("C:\\Users\\PhilipJunXian\\OneDrive\\Polit\\Master\\Social Data Science\\Opgave\\polit.csv")
#########################################################
### Clean the data for NA og hyperlink

data.EC = raw.data.EC %>% 
  filter( !is.na(o_kar)==TRUE)  %>% 
  select(-X, -id, -o_gns, -r_gns, -hyperlink)

#########################################################
### clean Ej mødt, Ikke bestået, Syg
### create aggregated grade variable

data.EC$s_kar = ifelse(is.na(data.EC$r_kar)==TRUE,data.EC$o_kar, data.EC$o_kar + data.EC$r_kar )
data.EC$kar.m = as.numeric(levels(data.EC$kar))[data.EC$kar]

data.EC=data.EC %>%  
  filter(!is.na(kar.m)==TRUE)
data.EC$kar.m = NULL

#########################################################
### create semester and year column
data.EC = data.EC %>% 
  separate(sem, c("semester", "year"), 7)

data.EC = as.data.frame(lapply(data.EC,function(x) 
  if(is.character(x)|is.factor(x)) gsub("Winter-","Fall",x) else x))

data.EC = as.data.frame(lapply(data.EC,function(x) 
  if(is.character(x)|is.factor(x)) gsub("Summer-","Spring",x) else x))

data.EC$semester = ifelse(is.na(data.EC$blok)==TRUE, as.character(data.EC$semester), "Summer" )

data.EC$blok = NULL


#########################################################
### correct grades and numeric variables

as.numeric(data.EC$o_kar)
as.numeric(data.EC$r_kar)
as.numeric(data.EC$s_kar)

revalue(data.EC$kar, c("00" = "0", "02" = "2", "-3" = "-3")) -> data.EC$kar 

data.EC$kar = data.EC$kar %>% 
  as.character() %>% 
  as.numeric()


#########################################################
#########################################################
#### preparing data frame for plotting graph

# select variables
data.EC.graf = data.EC %>% 
  select(year, semester, name, kar, s_kar)
data.EC.graf$product = data.EC.graf$s_kar * data.EC.graf$kar

data.EC.graf1 = aggregate(data.EC.graf$product, by=list(data.EC.graf$year, data.EC.graf$semester), FUN=sum, na.rm=TRUE)
colnames(data.EC.graf1)[3] = "sum.product" 

data.EC.graf2 = aggregate(data.EC.graf$s_kar, by=list(data.EC.graf$year,data.EC.graf$semester), FUN=sum, na.rm=TRUE)
colnames(data.EC.graf2)[3] = "count" 

data.EC.graf.plot = left_join(data.EC.graf1, data.EC.graf2)
colnames(data.EC.graf.plot)[1] = "year" 
colnames(data.EC.graf.plot)[2] = "semester" 
data.EC.graf.plot$average = data.EC.graf.plot$sum.product / data.EC.graf.plot$count 

### HUSK - fjern 2011, spring - ej retvisende: 1 fag
### HUSK - fjern 2016, summer - endnu ej realiserede karaktere: 2 fag

# data.EC.graf.plot1 = data.EC.graf.plot[!(data.EC.graf.plot$year=="2011" & data.EC.graf.plot$semester=="Spring"),]
# data.EC.graf.plot1 = data.EC.graf.plot1[!(data.EC.graf.plot1$year=="2016" & data.EC.graf.plot1$semester=="Summer"),] 

#########################################################
#########################################################
### create graph

