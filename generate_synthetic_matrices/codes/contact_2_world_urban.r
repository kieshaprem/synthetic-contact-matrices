library(countrycode)
library(rjags)
source('codes/functions_processContactmatrices.r')

load('input/pop/popUrban.rdata')

load('output/KAPPA_HOME.RData')
load('output/KAPPA_WORK.RData')
load('output/KAPPA_SCHOOL.RData')
load('output/KAPPA_OTHERS.RData')

load('output/hampolymod.rdata')
load('output/hamdhs_urban.rdata')
load('output/hamworld_urban.rdata')
load('input/work/workpopage_urban.rdata')

load('input/school/schoolage_urban.rdata')

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
  save(contact_home,file = 'output/syntheticcontactmatrices2020/urban/contact_home_urban.rdata')
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
  contact_work  = lapply(contact_work, function(x) round(x,5))
  save(contact_work,file = 'output/syntheticcontactmatrices2020/urban/contact_work_urban.rdata')
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
  contact_school  = lapply(contact_school, function(x) round(x,5))
  save(contact_school,file = 'output/syntheticcontactmatrices2020/urban/contact_school_urban.rdata')
}

if(OTHERS)
{
  contact_others = list()
  for(i in 1:length(countries))
  {
    pop = as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])+
      as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% countries[i],paste0('age',seq(0,75,5))][1,])
    popratio = pop/sum(pop)
    
    # print(i)
    if(countries[i] %in% POLYMODcountries) contact_others[[countries[i]]] = KAPPA_OTHERS[[countries[i]]][1:16,1:16]
    if(!(countries[i] %in% POLYMODcountries)) contact_others[[countries[i]]] = KAPPA_OTHERS$POLYMOD[1:16,1:16]*(as.numeric(popratio/polymodpopratio[1:16]))
    #%*%t(as.numeric(popratio[popratio$iso3c %in% countries[i],paste0('age',seq(0,75,5install.packages("npsp")))]/polymodpopratio)))
  }
  contact_others = lapply(contact_others, function(x) round(x,5))
  save(contact_others,file = 'output/syntheticcontactmatrices2020/urban/contact_others_urban.rdata')
}

if(ALL)
{
  contact_all = list()
  for(i in 1:length(countries))
  {
    print(i)
    contact_all[[countries[i]]] = contact_home[[countries[i]]]+contact_work[[countries[i]]]+contact_school[[countries[i]]]+contact_others[[countries[i]]]
  }   
  # contact_others = round(contact_others,5)
  save(contact_all,file = 'output/syntheticcontactmatrices2020/urban/contact_all_urban.rdata')
}


rm(contact_all,contact_home,contact_work,contact_school,contact_others)

load('output/syntheticcontactmatrices2020/overall/contact_all.rdata')
load('output/syntheticcontactmatrices2020/overall/contact_home.rdata')
load('output/syntheticcontactmatrices2020/overall/contact_work.rdata')
load('output/syntheticcontactmatrices2020/overall/contact_school.rdata')
load('output/syntheticcontactmatrices2020/overall/contact_others.rdata')

contacts2020 = list()
for(co in 1:length(contact_all))
{
  ISO = names(contact_all)[co]
  contacts2020[[ISO]]$home = contact_home[[ISO]]
  contacts2020[[ISO]]$work = contact_work[[ISO]]
  contacts2020[[ISO]]$school = contact_school[[ISO]]
  contacts2020[[ISO]]$others = contact_others[[ISO]]
  contacts2020[[ISO]]$all = contact_all[[ISO]]
}

rm(contact_all,contact_home,contact_work,contact_school,contact_others,co,ISO)



load('output/syntheticcontactmatrices2020/urban/contact_all_urban.rdata')
load('output/syntheticcontactmatrices2020/urban/contact_home_urban.rdata')
load('output/syntheticcontactmatrices2020/urban/contact_work_urban.rdata')
load('output/syntheticcontactmatrices2020/urban/contact_school_urban.rdata')
load('output/syntheticcontactmatrices2020/urban/contact_others_urban.rdata')

isolist = c('HKG','MAC','SGP')
for(ISO in isolist)
{
  contact_home[[ISO]] = contacts2020[[ISO]]$home
  contact_work[[ISO]] = contacts2020[[ISO]]$work
  contact_school[[ISO]] = contacts2020[[ISO]]$school
  contact_others[[ISO]] = contacts2020[[ISO]]$others
  contact_all[[ISO]] = contacts2020[[ISO]]$all
}


save(contact_all,file = 'output/syntheticcontactmatrices2020/urban/contact_all_urban.rdata')
save(contact_home,file = 'output/syntheticcontactmatrices2020/urban/contact_home_urban.rdata')
save(contact_work,file = 'output/syntheticcontactmatrices2020/urban/contact_work_urban.rdata')
save(contact_school,file = 'output/syntheticcontactmatrices2020/urban/contact_school_urban.rdata')
save(contact_others,file = 'output/syntheticcontactmatrices2020/urban/contact_others_urban.rdata')

