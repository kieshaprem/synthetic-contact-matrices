
library(countrycode)
options(scipen=999)
load('input/school/enrolment.rdata')
load('input/pop/popRural.rdata')
load('input/pop/popUrban.rdata')
load('input/pop/poptotal.rdata')


students = data.frame(iso = enrolment$iso,array(0,c(nrow(enrolment),16)))
colnames(students)[2:ncol(students)] = paste0('age',seq(0,75,5))
students_urban = students_rural = students

for(i in 1:nrow(students_urban))
{
  ISO = as.character(students_urban$iso[i])
  students_urban[i,2] = as.integer((popUrban$total_female[popUrban$total_female$iso3c %in% ISO,4]+popUrban$total_male[popUrban$total_male$iso3c %in% ISO,4])*(enrolment[enrolment$iso %in% ISO,2]/100))
  students_urban[i,3] = as.integer((popUrban$total_female[popUrban$total_female$iso3c %in% ISO,5]+popUrban$total_male[popUrban$total_male$iso3c %in% ISO,5])*(enrolment[enrolment$iso %in% ISO,3]/100))
  students_urban[i,4] = as.integer((popUrban$total_female[popUrban$total_female$iso3c %in% ISO,6]+popUrban$total_male[popUrban$total_male$iso3c %in% ISO,6])*(enrolment[enrolment$iso %in% ISO,4]/100))
  students_urban[i,5] = as.integer((popUrban$total_female[popUrban$total_female$iso3c %in% ISO,7]+popUrban$total_male[popUrban$total_male$iso3c %in% ISO,7])*(enrolment[enrolment$iso %in% ISO,5]/100))
  students_urban[i,6] = as.integer((popUrban$total_female[popUrban$total_female$iso3c %in% ISO,8]+popUrban$total_male[popUrban$total_male$iso3c %in% ISO,8])*(enrolment[enrolment$iso %in% ISO,6]/100))
  
  students_rural[i,2] = as.integer((popRural$total_female[popRural$total_female$iso3c %in% ISO,4]+popRural$total_male[popRural$total_male$iso3c %in% ISO,4])*(enrolment[enrolment$iso %in% ISO,2]/100))
  students_rural[i,3] = as.integer((popRural$total_female[popRural$total_female$iso3c %in% ISO,5]+popRural$total_male[popRural$total_male$iso3c %in% ISO,5])*(enrolment[enrolment$iso %in% ISO,3]/100))
  students_rural[i,4] = as.integer((popRural$total_female[popRural$total_female$iso3c %in% ISO,6]+popRural$total_male[popRural$total_male$iso3c %in% ISO,6])*(enrolment[enrolment$iso %in% ISO,4]/100))
  students_rural[i,5] = as.integer((popRural$total_female[popRural$total_female$iso3c %in% ISO,7]+popRural$total_male[popRural$total_male$iso3c %in% ISO,7])*(enrolment[enrolment$iso %in% ISO,5]/100))
  students_rural[i,6] = as.integer((popRural$total_female[popRural$total_female$iso3c %in% ISO,8]+popRural$total_male[popRural$total_male$iso3c %in% ISO,8])*(enrolment[enrolment$iso %in% ISO,6]/100))
  
}
rm(students,i,ISO)

save(students_rural,file = 'input/school/students_rural.rdata')
save(students_urban,file = 'input/school/students_urban.rdata')