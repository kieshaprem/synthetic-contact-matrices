library(countrycode)
library(rjags)
source('codes/functions_processContactmatrices.r')

load('input/pop/popratio.rdata')
load('input/pop/poptotal.rdata')
load('output/lambdas.rdata')
load('output/deltas.rdata')

load('output/hampolymod.rdata')
load('output/hamdhs.rdata')
load('output/hampolymoddhs.rdata')
load('output/hamworld.rdata')
load('input/work/workpopage.rdata')

load('input/school/schoolage.rdata')
load('input/polymod_pworkandsch.rdata')


# country codes
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS)
POLYMODandDHscountries = c(POLYMODcountries,DHScountries)
countries = sort(names(HAM_WORLD)[names(HAM_WORLD) %in% school$iso])
# countries = sort(names(HAM_WORLD)[names(HAM_WORLD) %in% school$Country.Code])

work = workpopage$total
polymodpop = colSums(poptotal[poptotal$iso3c %in% POLYMODcountries,4:25])
polymodpopratio = (polymodpop[grep(pattern = 'age',x = names(polymodpop))]/polymodpop['total'])[1:16]

signif(((sum(poptotal$total[poptotal$iso3c %in% countries])/7794798729)*100),3)
# 97.2% of the world in the 177 countries

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
  save(contact_home,file = 'output/syntheticmatrices/contact_home.rdata')
}

if(WORK)
{
  contact_work = list()
  for(i in 1:length(countries))
  {
    print(i)
    if(countries[i] %in% POLYMODcountries) contact_work[[countries[i]]] = lambdas[[countries[i]]]$work*getWorkPop(COUNTRY = countries[i],POLYMOD = TRUE)
    if(!(countries[i] %in% POLYMODcountries)) contact_work[[countries[i]]] = t(t(lambdas$POLYMOD$work*getWorkPop(COUNTRY = countries[i],POLYMOD = FALSE))*1)
    
  }
  contact_work = lapply(contact_work, function(x) round(x,5))
  save(contact_work,file = 'output/syntheticmatrices/contact_work.rdata')
}

if(SCHOOL)
{
  contact_school = list()
  for(i in 1:length(countries))
  {
    print(i)
    if(countries[i] %in% POLYMODcountries) contact_school[[countries[i]]] = lambdas[[countries[i]]]$school*getSchoolPop(COUNTRY = countries[i],POLYMOD = TRUE)
    if(!(countries[i] %in% POLYMODcountries)) contact_school[[countries[i]]] =  t(t(lambdas$POLYMOD$school*getSchoolPop(COUNTRY = countries[i],POLYMOD = FALSE)))
  }
  contact_school = lapply(contact_school, function(x) round(x,5))
  save(contact_school,file = 'output/syntheticmatrices/contact_school.rdata')
}

if(OTHERS)
{
  contact_others = list()
  for(i in 1:length(countries))
  {
    print(i)
    if(countries[i] %in% POLYMODcountries) contact_others[[countries[i]]] = lambdas[[countries[i]]]$others
    if(!(countries[i] %in% POLYMODcountries)) contact_others[[countries[i]]] = t(t(lambdas$POLYMOD$others)*(as.numeric(popratio[popratio$iso3c %in% countries[i],paste0('age',seq(0,75,5))]/polymodpopratio)))
  }
  
  contact_others = lapply(contact_others, function(x) round(x,5))
  save(contact_others,file = 'output/syntheticmatrices/contact_others.rdata')
}

if(ALL)
{
  contact_all = list()
  for(i in 1:length(countries))
  {
    print(i)
    contact_all[[countries[i]]] = contact_home[[countries[i]]]+contact_work[[countries[i]]]+contact_school[[countries[i]]]+contact_others[[countries[i]]]
  }   
  save(contact_all,file = 'output/syntheticmatrices/contact_all.rdata')
}
