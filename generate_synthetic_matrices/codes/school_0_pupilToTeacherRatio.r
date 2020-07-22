library(wbstats)
library(countrycode)
options(scipen=999)
str(wb_cachelist, max.level = 1)
# new_cache <- wbcache()
source('codes/functions_processContactmatrices.r')
load('input/pop/popratio.rdata')
load('input/work/workpopage.rdata')
work = workpopage$total

# Source: UNESCO Institute for Statistics ( uis.unesco.org )
# Pupil-teacher ratio (PTR)
# Definition: Average number of pupils per teacher at a given level of education, based on headcounts of both pupils and teachers.
# Calculation method: Divide the total number of pupils enrolled at the specified level of education by the number of teachers at the same level.
# Pupil-teacher ratio, preprimary: Preprimary school pupil-teacher ratio is the average number of pupils per teacher in preprimary school.
# Pupil-teacher ratio, primary: Primary school pupil-teacher ratio is the average number of pupils per teacher in primary school.
# Pupil-teacher ratio, secondary: Secondary school pupil-teacher ratio is the average number of pupils per teacher in secondary school.
# Pupil-teacher ratio, tertiary: Tertiary school pupil-teacher ratio is the average number of pupils per teacher in tertiary school.



indicators = c('SE.PRE.ENRL.TC.ZS','SE.PRM.ENRL.TC.ZS','SE.SEC.ENRL.TC.ZS','SE.TER.ENRL.TC.ZS')

allyears_PTRpreprimary=  wb(indicator = "SE.PRE.ENRL.TC.ZS", startdate = 2000, enddate = 2020)
allyears_PTRprimary=  wb(indicator = "SE.PRM.ENRL.TC.ZS", startdate = 2000, enddate = 2020)
allyears_PTRsecondary=  wb(indicator = "SE.SEC.ENRL.TC.ZS", startdate = 2000, enddate = 2020)
allyears_PTRtertiary=  wb(indicator = "SE.TER.ENRL.TC.ZS", startdate = 2000, enddate = 2020)

PTRpreprimary  = getLatestData_indicator(ALLDATA = allyears_PTRpreprimary)
PTRprimary  = getLatestData_indicator(ALLDATA = allyears_PTRprimary)
PTRsecondary  = getLatestData_indicator(ALLDATA = allyears_PTRsecondary)
PTRtertiary  = getLatestData_indicator(ALLDATA = allyears_PTRtertiary)

PTRlist = list(preprimary = PTRpreprimary,
                primary = PTRprimary,
                secondary = PTRsecondary,
                tertiary = PTRtertiary)


iso = as.character(PTRlist[[1]]$iso3c)
for(i in 2:length(PTRlist))
{
  extra = as.character(PTRlist[[i]]$iso3c)[!(as.character(PTRlist[[i]]$iso3c) %in% iso)]
  if(length(extra)>0) iso = c(iso,extra)
  iso = sort(iso)
}

iso
iso = iso[(iso %in% popratio$iso3c)]
iso = iso[(iso %in% work$iso3c)]

PTR_data = list()
for(j in 1:length(PTRlist))
{
  value = array(NA,length(iso))
  for(i in 1:length(iso))
  {
    index = which(PTRlist[[j]]$iso3c %in% iso[i])
    if(length(index)>0) value[i] = PTRlist[[j]]$value[index]
  }
  PTR_data[[j]] = value
}



PTR = data.frame(iso = iso,do.call(cbind.data.frame, PTR_data))
colnames(PTR)[2:ncol(PTR)] = names(PTRlist)



countrycode(PTR$iso[is.na(PTR$preprimary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in PTR$iso[is.na(PTR$preprimary)]) PTR$preprimary[PTR$iso %in% isocheck] = getRegionalMean(isocheck,DATA = PTRlist$preprimary)

countrycode(PTR$iso[is.na(PTR$primary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in PTR$iso[is.na(PTR$primary)]) PTR$primary[PTR$iso %in% isocheck] = getRegionalMean(isocheck,DATA = PTRlist$primary)

countrycode(PTR$iso[is.na(PTR$secondary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in PTR$iso[is.na(PTR$secondary)]) PTR$secondary[PTR$iso %in% isocheck] = getRegionalMean(isocheck,DATA = PTRlist$secondary)

countrycode(PTR$iso[is.na(PTR$tertiary)],origin = 'iso3c',destination = 'country.name')
for(isocheck in PTR$iso[is.na(PTR$tertiary)]) PTR$tertiary[PTR$iso %in% isocheck] = getRegionalMean(isocheck,DATA = PTRlist$tertiary)

save(PTR,file = 'input/school/pupiltoteacherratio.rdata')

# https://www.nationmaster.com/country-info/stats/Education/Teachers-as-percentage-of-labor-force
# 'SE.PRM.TCHR.FE.ZS'
