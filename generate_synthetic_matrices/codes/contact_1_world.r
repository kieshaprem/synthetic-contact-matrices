library(countrycode)
library(rjags)
source('codes/functions_processContactmatrices.r')

load('input/pop/popratio.rdata')
load('input/pop/poptotal.rdata')

load('output/KAPPA_HOME.RData')
load('output/KAPPA_WORK.RData')
load('output/KAPPA_SCHOOL.RData')
load('output/KAPPA_OTHERS.RData')

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
  save(contact_home,file = 'output/syntheticcontactmatrices2020/overall/contact_home.rdata')
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
  save(contact_work,file = 'output/syntheticcontactmatrices2020/overall/contact_work.rdata')
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
  save(contact_school,file = 'output/syntheticcontactmatrices2020/overall/contact_school.rdata')
}

if(OTHERS)
{
  contact_others = list()
  for(i in 1:length(countries))
  {
    # print(i)
    if(countries[i] %in% POLYMODcountries) contact_others[[countries[i]]] = KAPPA_OTHERS[[countries[i]]][1:16,1:16]
    if(!(countries[i] %in% POLYMODcountries)) contact_others[[countries[i]]] = KAPPA_OTHERS$POLYMOD[1:16,1:16]*
        (as.numeric(popratio[popratio$iso3c %in% countries[i],paste0('age',seq(0,75,5))]/polymodpopratio))#%*%t(as.numeric(popratio[popratio$iso3c %in% countries[i],paste0('age',seq(0,75,5))]/polymodpopratio)))
  }
  contact_others = lapply(contact_others, function(x) round(x,5))
  save(contact_others,file = 'output/syntheticcontactmatrices2020/overall/contact_others.rdata')
}

if(ALL)
{
  contact_all = list()
  for(i in 1:length(countries))
  {
    print(i)
    contact_all[[countries[i]]] = contact_home[[countries[i]]]+contact_work[[countries[i]]]+contact_school[[countries[i]]]+contact_others[[countries[i]]]
  }   
  save(contact_all,file = 'output/syntheticcontactmatrices2020/overall/contact_all.rdata')
}



save(contact_all,file = 'output/syntheticcontactmatrices2020/contact_all.rdata')
save(contact_home,file = 'output/syntheticcontactmatrices2020/contact_home.rdata')
save(contact_work,file = 'output/syntheticcontactmatrices2020/contact_work.rdata')
save(contact_school,file = 'output/syntheticcontactmatrices2020/contact_school.rdata')
save(contact_others,file = 'output/syntheticcontactmatrices2020/contact_others.rdata')





