library(countrycode)
library(rjags)
source('codes/functions_processContactmatrices.r')

load('input/pop/popRural.rdata')
load('output/lambdas.rdata')
load('output/deltas.rdata')

load('output/hampolymod.rdata')
load('output/hamdhs_rural.rdata')
load('output/hamworld_rural.rdata')
load('input/work/workpopage_rural.rdata')

load('input/school/schoolage_rural.rdata')
load('input/polymod_pworkandsch.rdata')

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
polymodpopratio = (polymodpop[grep(pattern = 'age',x = names(polymodpop))]/polymodpop['total'])[1:16]


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
  save(contact_home,file = 'output/syntheticmatrices/rural/contact_home_rural.rdata')
}

if(WORK)
{
  contact_work = list()
  for(i in 1:length(countries))
  {
    print(i)
    pop = as.numeric(popRural$total_female[popRural$total_female$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])+
      as.numeric(popRural$total_male[popRural$total_male$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])
    popratio = pop/sum(pop)
    
    if(countries[i] %in% POLYMODcountries) contact_work[[countries[i]]] = lambdas[[countries[i]]]$work*getWorkPop(COUNTRY = countries[i],POLYMOD = TRUE)
    if(!(countries[i] %in% POLYMODcountries)) contact_work[[countries[i]]] = t(t(lambdas$POLYMOD$work*getWorkPop(COUNTRY = countries[i],POLYMOD = FALSE))*1)
    
  }
  contact_work = lapply(contact_work, function(x) round(x,5))
  save(contact_work,file = 'output/syntheticmatrices/rural/contact_work_rural.rdata')
}

if(SCHOOL)
{
  contact_school = list()
  for(i in 1:length(countries))
  {
    print(i)
    pop = as.numeric(popRural$total_female[popRural$total_female$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])+
      as.numeric(popRural$total_male[popRural$total_male$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])
    popratio = pop/sum(pop)
    if(countries[i] %in% POLYMODcountries) contact_school[[countries[i]]] = lambdas[[countries[i]]]$school*getSchoolPop(COUNTRY = countries[i],POLYMOD = TRUE)
    if(!(countries[i] %in% POLYMODcountries)) contact_school[[countries[i]]] =  t(t(lambdas$POLYMOD$school*getSchoolPop(COUNTRY = countries[i],POLYMOD = FALSE)))
    
  }
  contact_school = lapply(contact_school, function(x) round(x,5))
  save(contact_school,file = 'output/syntheticmatrices/rural/contact_school_rural.rdata')
}

if(OTHERS)
{
  contact_others = list()
  for(i in 1:length(countries))
  {
    print(i)
    pop = as.numeric(popRural$total_female[popRural$total_female$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])+
      as.numeric(popRural$total_male[popRural$total_male$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])
    popratio = pop/sum(pop)
    
    if(countries[i] %in% POLYMODcountries) contact_others[[countries[i]]] = lambdas[[countries[i]]]$others
    if(!(countries[i] %in% POLYMODcountries)) contact_others[[countries[i]]] = t(t(lambdas$POLYMOD$others)*(as.numeric(popratio/polymodpopratio)))
  }
  contact_others = lapply(contact_others, function(x) round(x,5))
  save(contact_others,file = 'output/syntheticmatrices/rural/contact_others_rural.rdata')
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
  save(contact_all,file = 'output/syntheticmatrices/rural/contact_all_rural.rdata')
}

