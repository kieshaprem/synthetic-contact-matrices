library(countrycode)
options(scipen=999)
load('input/school/students.rdata')
load('input/school/teachers.rdata')
load('input/pop/poptotal.rdata')

school =  data.frame(iso = teachers$iso,array(0,c(nrow(teachers),16)))
colnames(school)[2:ncol(school)] = paste0('age',seq(0,75,5))

for(i in 1:nrow(school))
{
  ISO = as.character(school$iso[i])
  school[i,2:17] = (teachers[teachers$iso %in% ISO,2:17]+students[students$iso %in% ISO,2:17])/poptotal[poptotal$iso3c %in%ISO,4:19]
}

save(school,file = 'input/school/schoolage.rdata')

# test function 
# image(getSchoolPop(COUNTRY = ISO))
