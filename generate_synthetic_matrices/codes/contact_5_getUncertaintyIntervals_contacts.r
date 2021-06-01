library(countrycode)
library(rjags)
source('codes/contactmatricesworld_function.r')

load('input/pop/popratio.rdata')
load('input/pop/poptotal.rdata')

load('output/KAPPA_HOME.RData')
load('output/KAPPA_WORK.RData')
load('output/KAPPA_SCHOOL.RData')
load('output/KAPPA_OTHERS.RData')
load('output/posterior_uncertainty_interval.rdata')

load('output/hampolymod.rdata')
load('output/hamdhs.rdata')
load('output/hampolymoddhs.rdata')
load('output/hamworld.rdata')
load('input/work/workpopage.rdata')

load('input/school/schoolage.rdata')

# load('output/schoolupdated.RData')
# country codes
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS)
POLYMODandDHscountries = c(POLYMODcountries,DHScountries)
countries = sort(names(HAM_WORLD)[names(HAM_WORLD) %in% school$iso])
# countries = sort(names(HAM_WORLD)[names(HAM_WORLD) %in% school$Country.Code])

work = workpopage$total
polymodpop = colSums(poptotal[poptotal$iso3c %in% POLYMODcountries,4:25])
polymodpopratio = polymodpop[grep(pattern = 'age',x = names(polymodpop))]/polymodpop['total']

signif(((sum(poptotal$total[poptotal$iso3c %in% countries])/7794798729)*100),3)
# 97.2% of the world in the 177 countries

HOME = TRUE
WORK = TRUE
SCHOOL = TRUE 
OTHERS = TRUE
ALL = TRUE

inferred.hhcontact_uncertainty = function(HH,AGEMAX,MATRIX){
  data.matrix(MATRIX[1:AGEMAX,1:AGEMAX]*(HH[1:AGEMAX,1:AGEMAX]))
}
HHcontact_POLYMOD_DHS_uncertainty = function(COUNTRY,POLYMOD,MATRIX){
  HH = HAM_POLYMODandDHS[[COUNTRY]]
  if(POLYMOD==FALSE) contact_home = inferred.hhcontact_uncertainty(HH = HH,AGEMAX = 16,MATRIX)
  if(POLYMOD==TRUE) contact_home = data.matrix(KAPPA_HOME[[COUNTRY]][1:16,1:16]*(HH[1:16,1:16]))
  return(contact_home)
}

HHcontact_Rest_of_world_uncertainty = function(COUNTRY,MATRIX){
  HH = HAM_WORLD[[(COUNTRY)]]
  contact_home = inferred.hhcontact_uncertainty(HH = HH,AGEMAX = 16,MATRIX)
  return(contact_home)
}

getWorkPop = function(COUNTRY){
  workingpop = as.numeric(work[work$iso3c %in% COUNTRY,6:21])
  workingpop[1:3] =0
  workingpop[15:16]=0.01
  wa = (workingpop)%*%t(workingpop)
  return(wa)
}

getSchoolPop = function(COUNTRY){
  schoolpop = as.numeric(school[school$iso %in% COUNTRY,grep(pattern = 'age',x = colnames(school))])
  sa = (schoolpop)%*%t(schoolpop)+0.01
  sa = sa
  return(sa)
}
getSchoolPopulation = function(COUNTRY){
  schoolpop = as.numeric(school[school$iso %in% COUNTRY,grep(pattern = 'age',x = colnames(school))])
  sa = (schoolpop)+0.00#(schoolpop)%*%t(schoolpop)+0.01
  sa = sa
  return(sa)
}

# for example: the 10th percentile 
home_uncertainty = estimates_post_interval$p10$lh
work_uncertainty = estimates_post_interval$p10$lw
school_uncertainty = estimates_post_interval$p10$ls
others_uncertainty = estimates_post_interval$p10$lo


contact_home = list()
for(i in 1:length(countries))
{
  print(i)
  if(countries[i] %in% POLYMODcountries) contact_home[[countries[i]]] = HHcontact_POLYMOD_DHS_uncertainty(COUNTRY=countries[i],POLYMOD=TRUE,MATRIX = home_uncertainty)
  if(countries[i] %in% DHScountries) contact_home[[countries[i]]] = HHcontact_POLYMOD_DHS_uncertainty(COUNTRY=countries[i],POLYMOD=FALSE,MATRIX = home_uncertainty)
  if(!(countries[i] %in% POLYMODandDHscountries)) contact_home[[countries[i]]] = HHcontact_Rest_of_world_uncertainty(COUNTRY=countries[i],MATRIX = home_uncertainty)
}
contact_home = lapply(contact_home, function(x) round(x,5))


contact_work = list()
for(i in 1:length(countries))
{
  # print(i)
  if(countries[i] %in% POLYMODcountries) contact_work[[countries[i]]] = KAPPA_WORK[[countries[i]]][1:16,1:16]*getWorkPop(COUNTRY = countries[i])
  if(!(countries[i] %in% POLYMODcountries)) contact_work[[countries[i]]] = work_uncertainty[1:16,1:16]*getWorkPop(COUNTRY = countries[i])
}
contact_work = lapply(contact_work, function(x) round(x,5))


contact_school = list()
for(i in 1:length(countries))
{
  # print(i)
  if(countries[i] %in% POLYMODcountries) contact_school[[countries[i]]] = KAPPA_SCHOOL[[countries[i]]][1:16,1:16]*getSchoolPop(COUNTRY = countries[i])
  if(!(countries[i] %in% POLYMODcountries)) contact_school[[countries[i]]] = school_uncertainty[1:16,1:16]*getSchoolPop(COUNTRY = countries[i])
}
contact_school = lapply(contact_school, function(x) round(x,5))


contact_others = list()
for(i in 1:length(countries))
{
  # print(i)
  if(countries[i] %in% POLYMODcountries) contact_others[[countries[i]]] = KAPPA_OTHERS[[countries[i]]][1:16,1:16]
  if(!(countries[i] %in% POLYMODcountries)) contact_others[[countries[i]]] = others_uncertainty[1:16,1:16]*
      (as.numeric(popratio[popratio$iso3c %in% countries[i],paste0('age',seq(0,75,5))]/polymodpopratio))#%*%t(as.numeric(popratio[popratio$iso3c %in% countries[i],paste0('age',seq(0,75,5))]/polymodpopratio)))
}
contact_others = lapply(contact_others, function(x) round(x,5))


contact_all = list()
for(i in 1:length(countries))
{
  # print(i)
  contact_all[[countries[i]]] = contact_home[[countries[i]]]+contact_work[[countries[i]]]+contact_school[[countries[i]]]+contact_others[[countries[i]]]
}   


