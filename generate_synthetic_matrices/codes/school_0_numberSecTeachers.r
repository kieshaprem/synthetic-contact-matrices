library(wbstats)
library(countrycode)
options(scipen=999)
str(wb_cachelist, max.level = 1)
# new_cache <- wbcache()
source('codes/functions_processContactmatrices.r')
load('input/pop/popratio.rdata')
load('output/workpopage.rdata')
work = workpopage$total

# Source: UNESCO Institute for Statistics ( uis.unesco.org )
# Secondary education, teachers
# Definition: Secondary education teachers includes full-time and part-time teachers.


all_teacherssecondary=  wb(indicator = "SE.SEC.TCHR", startdate = 2000, enddate = 2020)
teacherssecondary  = getLatestData_indicator(ALLDATA = all_teacherssecondary)

secteachers = data.frame(iso = work$iso3c, number =NA)
for(i in 1:nrow(secteachers))
{
  ISO = as.character(secteachers$iso[i])
  if(sum(teacherssecondary$iso3c %in% ISO)>0) secteachers[i,2] = teacherssecondary$value[teacherssecondary$iso3c %in% ISO]
}

countrycode(secteachers$iso[is.na(secteachers$number)],origin = 'iso3c',destination = 'country.name')
# for(isocheck in secteachers$iso[is.na(secteachers$number)]) secteachers$number[secteachers$iso %in% isocheck] = getRegionalMean(isocheck,DATA = teacherssecondary)

save(secteachers,file = 'input/school/secteachers.rdata')
