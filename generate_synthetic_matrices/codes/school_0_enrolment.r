library(wbstats)
library(countrycode)
options(scipen=999)
str(wb_cachelist, max.level = 1)
# new_cache <- wbcache()
source('codes/functions_processContactmatrices.r')
load('input/pop/popratio.rdata')
load('input/work/workpopage.rdata')
work = workpopage$total

# IN.EDU.NET.ENRL.RATIO

indicators = c('SE.PRE.NENR','SE.PRM.NENR','SE.SEC.NENR','SE.TER.ENRR')

allyears_enrolpreprimary=  wb(indicator = "SE.PRE.NENR", startdate = 2000, enddate = 2020)
allyears_enrolprimary=  wb(indicator = "SE.PRM.NENR", startdate = 2000, enddate = 2020)
allyears_enrolsecondary=  wb(indicator = "SE.SEC.NENR", startdate = 2000, enddate = 2020)
allyears_enroltertiary=  wb(indicator = "SE.TER.ENRR", startdate = 2000, enddate = 2020)

enrolPreprimary  = getLatestData_indicator(ALLDATA = allyears_enrolpreprimary)
enrolPrimary  = getLatestData_indicator(ALLDATA = allyears_enrolprimary)
enrolSecondary  = getLatestData_indicator(ALLDATA = allyears_enrolsecondary)
enrolTertiary  = getLatestData_indicator(ALLDATA = allyears_enroltertiary)

enrolmentlist = list(preprimary = enrolPreprimary,
                     primary = enrolPrimary,
                     secondary = enrolSecondary,
                     tertiary = enrolTertiary)


iso = as.character(enrolmentlist[[1]]$iso3c)
for(i in 2:length(enrolmentlist))
{
  extra = as.character(enrolmentlist[[i]]$iso3c)[!(as.character(enrolmentlist[[i]]$iso3c) %in% iso)]
  if(length(extra)>0) iso = c(iso,extra)
  iso = sort(iso)
}

iso
iso = iso[(iso %in% popratio$iso3c)]
iso = iso[(iso %in% work$iso3c)]

enrolment_data = list()
for(j in 1:length(enrolmentlist))
{
  value = array(NA,length(iso))
  for(i in 1:length(iso))
  {
    index = which(enrolmentlist[[j]]$iso3c %in% iso[i])
    if(length(index)>0) value[i] = enrolmentlist[[j]]$value[index]
  }
  enrolment_data[[j]] = value
}



enrolment_levels = data.frame(iso = iso,do.call(cbind.data.frame, enrolment_data))
colnames(enrolment_levels)[2:ncol(enrolment_levels)] = names(enrolmentlist)



countrycode(enrolment_levels$iso[is.na(enrolment_levels$preprimary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in enrolment_levels$iso[is.na(enrolment_levels$preprimary)]) enrolment_levels$preprimary[enrolment_levels$iso %in% isocheck] = getRegionalMean(isocheck = isocheck,DATA = enrolPreprimary)

countrycode(enrolment_levels$iso[is.na(enrolment_levels$primary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in enrolment_levels$iso[is.na(enrolment_levels$primary)]) enrolment_levels$primary[enrolment_levels$iso %in% isocheck] = getRegionalMean(isocheck,DATA = enrolPrimary)

countrycode(enrolment_levels$iso[is.na(enrolment_levels$secondary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in enrolment_levels$iso[is.na(enrolment_levels$secondary)]) enrolment_levels$secondary[enrolment_levels$iso %in% isocheck] = getRegionalMean(isocheck,DATA = enrolSecondary)

countrycode(enrolment_levels$iso[is.na(enrolment_levels$tertiary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in enrolment_levels$iso[is.na(enrolment_levels$tertiary)]) enrolment_levels$tertiary[enrolment_levels$iso %in% isocheck] = getRegionalMean(isocheck,DATA = enrolTertiary)

# allyears_enrolpreprimary=  wb(indicator = "SE.PRM.AGES", startdate = 2000, enddate = 2020)
age_enrolprimary=  wb(indicator = "SE.PRM.AGES", startdate = 2000, enddate = 2020)
age_enrolsecondary=  wb(indicator = "SE.SEC.AGES", startdate = 2000, enddate = 2020)
compulsory_schooling=  wb(indicator = "SE.COM.DURS", startdate = 2000, enddate = 2020)
duration_secondary=  wb(indicator = "SE.SEC.DURS", startdate = 2000, enddate = 2020)




age_enrolprimary  = getLatestData_indicator(ALLDATA = age_enrolprimary)
age_enrolsecondary  = getLatestData_indicator(ALLDATA = age_enrolsecondary)
compulsory_schooling  = getLatestData_indicator(ALLDATA = compulsory_schooling)
duration_secondary  = getLatestData_indicator(ALLDATA = duration_secondary)


age_years_enrol = list(age_enrolprimary = age_enrolprimary,
                     age_enrolsecondary = age_enrolsecondary,
                     year_compulsory_schooling = compulsory_schooling,
                     duration_secondary = duration_secondary)


iso = as.character(age_years_enrol[[1]]$iso3c)
for(i in 2:length(age_years_enrol))
{
  extra = as.character(age_years_enrol[[i]]$iso3c)[!(as.character(age_years_enrol[[i]]$iso3c) %in% iso)]
  if(length(extra)>0) iso = c(iso,extra)
  iso = sort(iso)
}

iso
iso = iso[(iso %in% popratio$iso3c)]
iso = iso[(iso %in% work$iso3c)]
iso = iso[(iso %in% enrolment_levels$iso)]
iso = unique(iso)

age_years_data = list()
for(j in 1:length(age_years_enrol))
{
  value = array(NA,length(iso))
  for(i in 1:length(iso))
  {
    index = which(age_years_enrol[[j]]$iso3c %in% iso[i])
    if(length(index)>0) value[i] = age_years_enrol[[j]]$value[index]
  }
  age_years_data[[j]] = value
}



age_years_schooling = data.frame(iso = iso,do.call(cbind.data.frame, age_years_data))
colnames(age_years_schooling)[2:ncol(age_years_schooling)] = names(age_years_enrol)


countrycode(age_years_schooling$iso[is.na(age_years_schooling$age_enrolprimary)],origin = 'iso3c',destination = 'country.name')
# for(isocheck in age_years_schooling$iso[is.na(age_years_schooling$age_enrolprimary)]) age_years_schooling$age_enrolprimary[age_years_schooling$iso %in% isocheck] = getRegionalMean(isocheck = isocheck,DATA = age_enrolprimary)

countrycode(age_years_schooling$iso[is.na(age_years_schooling$age_enrolsecondary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in age_years_schooling$iso[is.na(age_years_schooling$age_enrolsecondary)]) age_years_schooling$age_enrolsecondary[age_years_schooling$iso %in% isocheck] = getRegionalMean(isocheck,DATA = age_enrolsecondary)
age_years_schooling$age_enrolsecondary = as.integer(age_years_schooling$age_enrolsecondary)

countrycode(age_years_schooling$iso[is.na(age_years_schooling$year_compulsory_schooling)],origin = 'iso3c',destination = 'country.name')
for(isocheck in age_years_schooling$iso[is.na(age_years_schooling$year_compulsory_schooling)]) age_years_schooling$year_compulsory_schooling[age_years_schooling$iso %in% isocheck] = getRegionalMean(isocheck,DATA = compulsory_schooling)
age_years_schooling$year_compulsory_schooling = as.integer(age_years_schooling$year_compulsory_schooling)

countrycode(age_years_schooling$iso[is.na(age_years_schooling$duration_secondary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in age_years_schooling$iso[is.na(age_years_schooling$duration_secondary)]) age_years_schooling$duration_secondary[age_years_schooling$iso %in% isocheck] = getRegionalMean(isocheck,DATA = compulsory_schooling)
age_years_schooling$duration_secondary = as.integer(age_years_schooling$duration_secondary)


enrolment_singleage = data.frame(iso = iso, array(0, c(length(iso),25)))
colnames(enrolment_singleage)[-1] = paste0('age',0:24)
start_age_school = end_age_school = data.frame(iso = iso, array(0, c(length(iso),4)))
colnames(start_age_school)[-1] = names(enrolmentlist)
colnames(end_age_school)[-1] = names(enrolmentlist)

for(i in 1:nrow(enrolment_singleage))
{
  ISO = enrolment_singleage$iso[i]
  enrolment_levels$preprimary[enrolment_levels$iso %in% ISO]
  start_agepreprimary= 3
  end_agepreprimary = (age_years_schooling$age_enrolprimary[age_years_schooling$iso %in% ISO])-1
  start_ageprimary = (age_years_schooling$age_enrolprimary[age_years_schooling$iso %in% ISO])
  end_ageprimary = (age_years_schooling$age_enrolsecondary[age_years_schooling$iso %in% ISO])-1
  start_agesecondary = (age_years_schooling$age_enrolsecondary[age_years_schooling$iso %in% ISO])
  end_agesecondary = (age_years_schooling$age_enrolsecondary[age_years_schooling$iso %in% ISO])+(age_years_schooling$duration_secondary[age_years_schooling$iso %in% ISO])-1
  start_agetertiary = (age_years_schooling$age_enrolsecondary[age_years_schooling$iso %in% ISO])+(age_years_schooling$duration_secondary[age_years_schooling$iso %in% ISO])
  end_agetertiary = start_agetertiary+3 # assume 4 years
  # preprimary 
  enrolment_singleage[i,paste0('age',start_agepreprimary:end_agepreprimary)] =  enrolment_levels$preprimary[enrolment_levels$iso %in% ISO]
  # primary  
  enrolment_singleage[i,paste0('age',start_ageprimary:end_ageprimary)] =  enrolment_levels$primary[enrolment_levels$iso %in% ISO]
  # secondary 
  enrolment_singleage[i,paste0('age',start_agesecondary:end_agesecondary)] =  enrolment_levels$secondary[enrolment_levels$iso %in% ISO]
  # tertiary  
  enrolment_singleage[i,paste0('age',start_agetertiary:end_agetertiary)] =  enrolment_levels$tertiary[enrolment_levels$iso %in% ISO]
  start_age_school[i,2] = start_agepreprimary
  start_age_school[i,3] = start_ageprimary
  start_age_school[i,4] = start_agesecondary
  start_age_school[i,5] = start_agetertiary
  
  end_age_school[i,2] = end_agepreprimary
  end_age_school[i,3] = end_ageprimary
  end_age_school[i,4] = end_agesecondary
  end_age_school[i,5] = end_agetertiary
}

enrolment_singleage
enrolment = data.frame(iso = enrolment_singleage$iso, array(0, c(length(iso),5)))
colnames(enrolment)[-1] = paste0('age',seq(0,20,5),'to',seq(4,24,5))

for(i in 1:nrow(enrolment))
{
  enrolment[i,2] = mean(as.numeric(enrolment_singleage[i,paste0('age',0:4)]))
  enrolment[i,3] = mean(as.numeric(enrolment_singleage[i,paste0('age',5:9)]))
  enrolment[i,4] = mean(as.numeric(enrolment_singleage[i,paste0('age',10:14)]))
  enrolment[i,5] = mean(as.numeric(enrolment_singleage[i,paste0('age',15:19)]))
  enrolment[i,6] = mean(as.numeric(enrolment_singleage[i,paste0('age',20:24)]))
}
save(enrolment_levels,file = 'input/school/enrolment_levels.rdata')
save(enrolment,file = 'input/school/enrolment.rdata')
save(end_age_school,file = 'input/school/end_age_school.rdata')
save(start_age_school,file = 'input/school/start_age_school.rdata')
