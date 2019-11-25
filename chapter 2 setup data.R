library(tidyverse)

aggdata = read.csv("strategiesfinal.csv")
#timeseries = read.csv("datatimeseries.csv")

temp = aggdata
temp$game = as.factor(temp$game)
temp$agerank = as.factor(temp$agerank)
temp$bornvillage = as.factor(temp$bornvillage)
temp$age = scale(temp$age,center = F)
temp$family = scale(temp$family,center = F)
temp$lantot = scale(temp$lantot,center = F)
temp$endmon = scale(temp$endmon,center = F)

#temp$family = round(temp$family*4.746779,0)
#temp$lantot = round(temp$lantot*4.512394,0)
#temp$endmon = round(temp$endmon*90.9262,0)

temp$game = factor(temp$game, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
temp$occupation = factor(temp$occupation, levels = c("Farming","Fishing","Oilpalm Smallholding",
                                                     "Others"))
temp$ethnicity = factor(temp$ethnicity, levels = c("Kutai","Dayak"))
temp$education = factor(temp$education, levels = c("Low","Medium","High"))
temp$sex = factor(temp$sex, levels = c("Male","Female"))
temp$agerank = factor(temp$agerank, levels = c("1","2","3","4"))

data = temp

rm(list=setdiff(ls(envir = .GlobalEnv), c("data"))) 

save.image("chapter3.RData")
