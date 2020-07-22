library(countrycode)
options(scipen=999)
load('input/school/students_urban.rdata')
load('input/school/teachers_urban.rdata')
load('input/pop/popUrban.rdata')

school_urban =  data.frame(iso = teachers_urban$iso,array(0,c(nrow(teachers_urban),16)))
colnames(school_urban)[2:ncol(school_urban)] = paste0('age',seq(0,75,5))

for(i in 1:nrow(school_urban))
{
  ISO = as.character(school_urban$iso[i])
  school_urban[i,2:17] = (teachers_urban[teachers_urban$iso %in% ISO,2:17]+
                            students_urban[students_urban$iso %in% ISO,2:17])/(popUrban$total_female[popUrban$total_female$iso3c %in%ISO,4:19]+popUrban$total_male[popUrban$total_male$iso3c %in%ISO,4:19])
}

save(school_urban,file = 'input/school/schoolage_urban.rdata')
rm(popUrban,students_urban,teachers_urban,i,ISO)
load('input/school/students_rural.rdata')
load('input/school/teachers_rural.rdata')
load('input/pop/popRural.rdata')

school_rural =  data.frame(iso = teachers_rural$iso,array(0,c(nrow(teachers_rural),16)))
colnames(school_rural)[2:ncol(school_rural)] = paste0('age',seq(0,75,5))

for(i in 1:nrow(school_rural))
{
  ISO = as.character(school_rural$iso[i])
  school_rural[i,2:17] = (teachers_rural[teachers_rural$iso %in% ISO,2:17]+
                            students_rural[students_rural$iso %in% ISO,2:17])/(popRural$total_female[popRural$total_female$iso3c %in%ISO,4:19]+popRural$total_male[popRural$total_male$iso3c %in%ISO,4:19])
}

save(school_rural,file = 'input/school/schoolage_rural.rdata')

# test function 
# image(getSchoolPop(COUNTRY = ISO))
