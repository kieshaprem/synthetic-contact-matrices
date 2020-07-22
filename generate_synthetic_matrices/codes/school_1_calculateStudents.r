
library(countrycode)
options(scipen=999)
load('input/school/enrolment.rdata')
load('input/pop/poptotal.rdata')
# source('codes/contactmatricesworld_function.r')

students = data.frame(iso = enrolment$iso,array(0,c(nrow(enrolment),16)))
colnames(students)[2:ncol(students)] = paste0('age',seq(0,75,5))

for(i in 1:nrow(students))
{
  ISO = as.character(students$iso[i])
  students[i,2] = as.integer(poptotal[poptotal$iso3c %in% ISO,4]*(enrolment[enrolment$iso %in% ISO,2]/100))
  students[i,3] = as.integer(poptotal[poptotal$iso3c %in% ISO,5]*(enrolment[enrolment$iso %in% ISO,3]/100))
  students[i,4] = as.integer(poptotal[poptotal$iso3c %in% ISO,6]*(enrolment[enrolment$iso %in% ISO,4]/100))
  students[i,5] = as.integer(poptotal[poptotal$iso3c %in% ISO,7]*(enrolment[enrolment$iso %in% ISO,5]/100))
  students[i,6] = as.integer(poptotal[poptotal$iso3c %in% ISO,8]*(enrolment[enrolment$iso %in% ISO,6]/100))
}

save(students,file = 'input/school/students.rdata')
