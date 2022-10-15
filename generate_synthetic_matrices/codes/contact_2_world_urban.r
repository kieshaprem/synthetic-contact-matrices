library(countrycode)
library(rjags)
source('codes/functions_processContactmatrices.r')

load('input/pop/popUrban.rdata')

load('output/lambdas.rdata')
load('output/deltas.rdata')

load('output/hampolymod.rdata')
load('output/hamdhs_urban.rdata')
load('output/hamworld_urban.rdata')
load('input/work/workpopage_urban.rdata')

load('input/school/schoolage_urban.rdata')
load('input/polymod_pworkandsch.rdata')

# load('output/schoolupdated.RData')
# country codes
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS_URBAN)
POLYMODandDHscountries = c(POLYMODcountries,DHScountries)
HAM_WORLD = HAM_WORLD_URBAN
HAM_POLYMODandDHS = c(HAM_POLYMOD[1:8],HAM_DHS_URBAN)
countries = sort(names(HAM_WORLD)[names(HAM_WORLD) %in% school_urban$iso])
# countries = sort(names(HAM_WORLD)[names(HAM_WORLD) %in% school$Country.Code])

work = workpopage_urban$total
school = school_urban
polymodpop = colSums(popUrban$total_female[popUrban$total_female$iso3c %in% POLYMODcountries,4:25]+
                       popUrban$total_male[popUrban$total_male$iso3c %in% POLYMODcountries,4:25])
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
  save(contact_home,file = 'output/syntheticmatrices/urban/contact_home_urban.rdata')
}

if(WORK)
{
  contact_work = list()
  for(i in 1:length(countries))
  {
    print(i)
    pop = as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])+
      as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])
    popratio = pop/sum(pop)
    
    if(countries[i] %in% POLYMODcountries) contact_work[[countries[i]]] = lambdas[[countries[i]]]$work*getWorkPop(COUNTRY = countries[i],POLYMOD = TRUE)
    if(!(countries[i] %in% POLYMODcountries)) contact_work[[countries[i]]] = t(t(lambdas$POLYMOD$work*getWorkPop(COUNTRY = countries[i],POLYMOD = FALSE))*
                                                                                 (as.numeric(popratio/polymodpopratio)))
  }
  contact_work = lapply(contact_work, function(x) round(x,5))
  save(contact_work,file = 'output/syntheticmatrices/urban/contact_work_urban.rdata')
}

if(SCHOOL)
{
  contact_school = list()
  for(i in 1:length(countries))
  {
    print(i)
    pop = as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])+
      as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])
    popratio = pop/sum(pop)
    if(countries[i] %in% POLYMODcountries) contact_school[[countries[i]]] = lambdas[[countries[i]]]$school*getSchoolPop(COUNTRY = countries[i],POLYMOD = TRUE)
    if(!(countries[i] %in% POLYMODcountries)) contact_school[[countries[i]]] =  t(t(lambdas$POLYMOD$school*getSchoolPop(COUNTRY = countries[i],POLYMOD = FALSE))*
                                                                                    (as.numeric(popratio/polymodpopratio)))
  }
  contact_school = lapply(contact_school, function(x) round(x,5))
  save(contact_school,file = 'output/syntheticmatrices/urban/contact_school_urban.rdata')
}

if(OTHERS)
{
  contact_others = list()
  for(i in 1:length(countries))
  {
    print(i)
    pop = as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])+
      as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])
    popratio = pop/sum(pop)
    
    if(countries[i] %in% POLYMODcountries) contact_others[[countries[i]]] = lambdas[[countries[i]]]$others
    if(!(countries[i] %in% POLYMODcountries)) contact_others[[countries[i]]] = t(t(lambdas$POLYMOD$others)*(as.numeric(popratio/polymodpopratio)))
  }
  contact_others = lapply(contact_others, function(x) round(x,5))
  save(contact_others,file = 'output/syntheticmatrices/urban/contact_others_urban.rdata')
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
  save(contact_all,file = 'output/syntheticmatrices/urban/contact_all_urban.rdata')
}


rm(contact_all,contact_home,contact_work,contact_school,contact_others)

load('output/syntheticmatrices/contact_all.rdata')
load('output/syntheticmatrices/contact_home.rdata')
load('output/syntheticmatrices/contact_work.rdata')
load('output/syntheticmatrices/contact_school.rdata')
load('output/syntheticmatrices/contact_others.rdata')

contacts2021 = list()
for(co in 1:length(contact_all))
{
  ISO = names(contact_all)[co]
  contacts2021[[ISO]]$home = contact_home[[ISO]]
  contacts2021[[ISO]]$work = contact_work[[ISO]]
  contacts2021[[ISO]]$school = contact_school[[ISO]]
  contacts2021[[ISO]]$others = contact_others[[ISO]]
  contacts2021[[ISO]]$all = contact_all[[ISO]]
}

rm(contact_all,contact_home,contact_work,contact_school,contact_others,co,ISO)



load('output/syntheticmatrices/urban/contact_all_urban.rdata')
load('output/syntheticmatrices/urban/contact_home_urban.rdata')
load('output/syntheticmatrices/urban/contact_work_urban.rdata')
load('output/syntheticmatrices/urban/contact_school_urban.rdata')
load('output/syntheticmatrices/urban/contact_others_urban.rdata')

isolist = c('HKG','MAC','SGP')
for(ISO in isolist)
{
  contact_home[[ISO]] = contacts2021[[ISO]]$home
  contact_work[[ISO]] = contacts2021[[ISO]]$work
  contact_school[[ISO]] = contacts2021[[ISO]]$school
  contact_others[[ISO]] = contacts2021[[ISO]]$others
  contact_all[[ISO]] = contacts2021[[ISO]]$all
}


save(contact_all,file = 'output/syntheticmatrices/urban/contact_all_urban.rdata')
save(contact_home,file = 'output/syntheticmatrices/urban/contact_home_urban.rdata')
save(contact_work,file = 'output/syntheticmatrices/urban/contact_work_urban.rdata')
save(contact_school,file = 'output/syntheticmatrices/urban/contact_school_urban.rdata')
save(contact_others,file = 'output/syntheticmatrices/urban/contact_others_urban.rdata')

