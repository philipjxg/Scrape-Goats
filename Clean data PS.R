library("readr")
library("purrr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("plyr")

raw.data=read.csv("C:\\Users\\PhilipJunXian\\OneDrive\\Polit\\Master\\Social Data Science\\Opgave\\statpolit_data.csv")

raw.data.PS = raw.data %>% 
  filter(study =="Political Science")

raw.data.EC = raw.data %>% 
  filter(study =="Economics")

#########################################################
#########################################################


names(raw.data.PS)
head(raw.data.PS)
summary(raw.data.PS)

data.PS = raw.data.PS %>% 
  select(name, sem, blok, kar, o_kar, r_kar)

names(data.PS)

dataB = data.PS

# sem: change summer to spring in "sem"
# blok: B5 Summerschool   

dataB = data.PS %>% 
  filter( !is.na(o_kar)==TRUE)  

#########################################################
### clean Ej mødt, Ikke bestået, Syg
### create aggregated grade variable

data1 = dataB

data1$s_kar = ifelse(is.na(data1$r_kar)==TRUE,data1$o_kar, data1$o_kar + data1$r_kar )
data1$kar.m = as.numeric(levels(data1$kar))[data1$kar]

data1=data1 %>%  
  filter(!is.na(kar.m)==TRUE)
data1$kar.m = NULL

#########################################################
### create semester and year column
data1 = data1 %>% 
  separate(sem, c("semester", "year"), 7)

data1 = as.data.frame(lapply(data1,function(x) 
  if(is.character(x)|is.factor(x)) gsub("Winter-","Fall",x) else x))

data1 = as.data.frame(lapply(data1,function(x) 
  if(is.character(x)|is.factor(x)) gsub("Summer-","Spring",x) else x))

data1$semester = ifelse(is.na(data1$blok)==TRUE, as.character(data1$semester), "Summer" )

data1$blok = NULL


#########################################################
### correct grades and numeric variables

as.numeric(data1$o_kar)
as.numeric(data1$r_kar)
as.numeric(data1$s_kar)

revalue(data1$kar, c("00" = "0", "02" = "2", "-3" = "-3")) -> data1$kar 

data1$kar = data1$kar %>% 
  as.character() %>% 
  as.numeric()



#########################################################
#########################################################
#### preparing data frame for plotting graph

# select variables
data.graf = data1 %>% 
  select(year, semester, name, kar, s_kar)
data.graf$product = data.graf$s_kar * data.graf$kar

data.graf1 = aggregate(data.graf$product, by=list(data.graf$year, data.graf$semester), FUN=sum, na.rm=TRUE)
colnames(data.graf1)[3] = "sum.product" 

data.graf2 = aggregate(data.graf$s_kar, by=list(data.graf$year,data.graf$semester), FUN=sum, na.rm=TRUE)
colnames(data.graf2)[3] = "count" 

data.graf.plot = left_join(data.graf1, data.graf2)
colnames(data.graf.plot)[1] = "year" 
colnames(data.graf.plot)[2] = "semester" 
data.graf.plot$average = data.graf.plot$sum.product / data.graf.plot$count 

data.graf.plot = as.data.frame(data.graf.plot %>% 
                                 arrange(year, semester.order))

### HUSK - fjern 2011, spring - ej retvisende: 1 fag
### HUSK - fjern 2016, summer - endnu ej realiserede karaktere: 2 fag

data.graf.plot1 = data.graf.plot[!(data.graf.plot$year=="2011" & data.graf.plot$semester=="Spring"),]
data.graf.plot1 = data.graf.plot1[!(data.graf.plot1$year=="2016" & data.graf.plot1$semester=="Summer"),] 

#########################################################
#########################################################
### create graph

graph = ggplot(data = data.graf.plot1, aes(year, average, fill=semester, color=semester)) + geom_line()   
graph

#########################################################
#########################################################
#### table over number of courses at a given year and semester  

courses = aggregate(data.graf$s_kar, list(data.graf$year, data.graf$semester, data.graf$name), FUN=sum, na.rm = TRUE)
courses$count = 1

Table = aggregate(courses$count, list(courses$Group.1, courses$Group.2), FUN=sum, na.rm = TRUE)








