Polit <-read_csv("/Users/frederikkehaider/Documents/SDS/polit.csv")

warnings()

#Sorterer på eksamensform= Laver en ny variabel. 
install.packages("stringr")
library("stringr")
library("dplyr")
library("tidytext")
library("plyr")


Language = Polit 
  Language$language_indicator = ifelse( str_detect(language1$name, "English") ==1, 1, 
                                         ifelse( str_detect(language1$name, "ENG") ==1, 1,
                                                 ifelse( str_detect(language1$name, "english") ==1, 1,
                                                 ifelse( str_detect(language1$name, "GB") ==1, 1, 0))))

