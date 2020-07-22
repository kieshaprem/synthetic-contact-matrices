library(countrycode)
library(rjags)
source('codes/functions_processContactmatrices.r')

load('input/pop/popRural.rdata')

load('output/KAPPA_HOME.RData')
load('output/KAPPA_WORK.RData')
load('output/KAPPA_SCHOOL.RData')
load('output/KAPPA_OTHERS.RData')

load('output/hampolymod.rdata')
load('output/hamdhs_rural.rdata')
load('output/hamworld_rural.rdata')
load('input/work/workpopage_rural.rdata')

load('input/school/schoolage_rural.rdata')

# country codes
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS_RURAL)
POLYMODandDHscountries = c(POLYMODcountries,DHScountries)
HAM_WORLD = HAM_WORLD_RURAL
HAM_POLYMODandDHS = c(HAM_POLYMOD[1:8],HAM_DHS_RURAL)
countries = sort(names(HAM_WORLD)[names(HAM_WORLD) %in% school_rural$iso])
countries = countries[countries %in% popRural$total_female$iso3c[popRural$total_female$total > 0]]

work = workpopage_rural$total
school = school_rural
polymodpop = colSums(popRural$total_female[popRural$total_female$iso3c %in% POLYMODcountries,4:25]+
                       popRural$total_male[popRural$total_male$iso3c %in% POLYMODcountries,4:25])
polymodpop['total'] = sum(polymodpop[1:21])
polymodpopratio = polymodpop[grep(pattern = 'age',x = names(polymodpop))]/polymodpop['total']


HOME = TRUE
WORK = TRUE
SCHOOL = TRUE 
OTHERS = TRUE
ALL = TRUE

if(HOME)
{
  contact_home = list()
  for(i in 1:length(countries))
  {
    print(i)
    if(countries[i] %in% POLYMODcountries) contact_home[[countries[i]]] = HHcontact_POLYMOD_DHS(COUNTRY=countries[i],POLYMOD=TRUE)
    if(countries[i] %in% DHScountries) contact_home[[countries[i]]] = HHcontact_POLYMOD_DHS(COUNTRY=countries[i],POLYMOD=FALSE)
    if(!(countries[i] %in% POLYMODandDHscountries)) contact_home[[countries[i]]] = HHcontact_Rest_of_world(COUNTRY=countries[i])
  }
  contact_home = lapply(contact_home, function(x) round(x,5))
  save(contact_home,file = 'output/syntheticcontactmatrices2020/rural/contact_home_rural.rdata')
}

if(WORK)
{
  contact_work = list()
  for(i in 1:length(countries))
  {
    print(i)
    if(countries[i] %in% POLYMODcountries) contact_work[[countries[i]]] = KAPPA_WORK[[countries[i]]][1:16,1:16]*getWorkPop(COUNTRY = countries[i])
    if(!(countries[i] %in% POLYMODcountries)) contact_work[[countries[i]]] = KAPPA_WORK$POLYMOD[1:16,1:16]*getWorkPop(COUNTRY = countries[i])
  }
  contact_work = lapply(contact_work, function(x) round(x,5))
  save(contact_work,file = 'output/syntheticcontactmatrices2020/rural/contact_work_rural.rdata')
}

if(SCHOOL)
{
  contact_school = list()
  for(i in 1:length(countries))
  {
    print(i)
    if(countries[i] %in% POLYMODcountries) contact_school[[countries[i]]] = KAPPA_SCHOOL[[countries[i]]][1:16,1:16]*getSchoolPop(COUNTRY = countries[i])
    if(!(countries[i] %in% POLYMODcountries)) contact_school[[countries[i]]] = KAPPA_SCHOOL$POLYMOD[1:16,1:16]*getSchoolPop(COUNTRY = countries[i])
  }
  contact_school = lapply(contact_school, function(x) round(x,5))
  save(contact_school,file = 'output/syntheticcontactmatrices2020/rural/contact_school_rural.rdata')
}

if(OTHERS)
{
  contact_others = list()
  for(i in 1:length(countries))
  {
    pop = as.numeric(popRural$total_female[popRural$total_female$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])+
      as.numeric(popRural$total_male[popRural$total_male$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])
    popratio = pop/sum(pop)
    
    # print(i)
    if(countries[i] %in% POLYMODcountries) contact_others[[countries[i]]] = KAPPA_OTHERS[[countries[i]]][1:16,1:16]
    if(!(countries[i] %in% POLYMODcountries)) contact_others[[countries[i]]] = KAPPA_OTHERS$POLYMOD[1:16,1:16]*(as.numeric(popratio/polymodpopratio[1:16]))
    #%*%t(as.numeric(popratio[popratio$iso3c %in% countries[i],paste0('age',seq(0,75,5))]/polymodpopratio)))
  }
  contact_others = lapply(contact_others, function(x) round(x,5))
  save(contact_others,file = 'output/syntheticcontactmatrices2020/rural/contact_others_rural.rdata')
}

if(ALL)
{
  contact_all = list()
  for(i in 1:length(countries))
  {
    print(i)
    contact_all[[countries[i]]] = contact_home[[countries[i]]]+contact_work[[countries[i]]]+contact_school[[countries[i]]]+contact_others[[countries[i]]]
  }   
  contact_all = lapply(contact_all, function(x) round(x,5))
  save(contact_all,file = 'output/syntheticcontactmatrices2020/rural/contact_all_rural.rdata')
}

