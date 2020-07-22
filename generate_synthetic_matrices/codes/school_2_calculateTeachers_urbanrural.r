library(countrycode)
options(scipen=999)
load('input/school/students_urban.rdata')
load('input/school/pupiltoteacherratio_urban.rdata')
load('input/school/secteachers.rdata')
load('input/pop/popUrban.rdata')
load('input/work/workpopage_urban.rdata')
load('input/school/proportion_teacheragelevel.rdata')
load('input/school/addpartime_teachers.rdata')
load('input/school/start_age_school.rdata')
load('input/school/end_age_school.rdata')
load('input/school/enrolment_levels.rdata')
source('codes/functions_processContactmatrices.r')

work = workpopage_urban$total
work[is.na(work)] = 0
iso = students_urban$iso
iso = iso[iso %in% PTR_urban$iso]
teachers_bylevel = data.frame(iso = iso,preprimary =NA, primary = NA,secondary = NA,tertiary = NA)

for(i in 1:nrow(teachers_bylevel))
{
  ISO = as.character(teachers_bylevel$iso[i])
  pop = popUrban$total_female[popUrban$total_female$iso3c %in%ISO,4:19]+popUrban$total_male[popUrban$total_male$iso3c %in%ISO,4:19]
  student_preprimary = as.integer(pop['age0']/5*
                                    (4-start_age_school$preprimary[start_age_school$iso %in% ISO]+1)*
                                    (enrolment_levels$preprimary[enrolment_levels$iso %in% ISO]/100))
  if(end_age_school$preprimary[end_age_school$iso %in% ISO]>4) 
  {
    student_preprimary = student_preprimary + as.integer(pop['age5']/5*
                                                           (end_age_school$preprimary[end_age_school$iso %in% ISO]-5+1)*
                                                           (enrolment_levels$preprimary[enrolment_levels$iso %in% ISO]/100))
  }
  student_primary = 0
  if(start_age_school$primary[start_age_school$iso %in% ISO]>=5&start_age_school$primary[start_age_school$iso %in% ISO]<10) 
  {
    student_primary = student_primary + as.integer(pop['age5']/5*
                                                     (9-start_age_school$primary[start_age_school$iso %in% ISO]+1)*
                                                     (enrolment_levels$primary[enrolment_levels$iso %in% ISO]/100)) 
  }
  if(end_age_school$primary[end_age_school$iso %in% ISO]>9) 
  {
    student_primary = student_primary + as.integer(pop['age10']/5*
                                                     ifelse((end_age_school$primary[end_age_school$iso %in% ISO]-10+1)>5,5,(end_age_school$primary[end_age_school$iso %in% ISO]-10+1))*
                                                     (enrolment_levels$primary[enrolment_levels$iso %in% ISO]/100))
  }
  student_secondary = 0
  if(start_age_school$secondary[start_age_school$iso %in% ISO]>=10&start_age_school$secondary[start_age_school$iso %in% ISO]<15) 
  {
    student_secondary = student_secondary + as.integer(pop['age10']/5*
                                                         (14-start_age_school$secondary[start_age_school$iso %in% ISO]+1)*
                                                         (enrolment_levels$secondary[enrolment_levels$iso %in% ISO]/100)) 
  }
  if(end_age_school$secondary[end_age_school$iso %in% ISO]>14) 
  {
    student_secondary = student_secondary + as.integer(pop['age15']/5*
                                                         ifelse((end_age_school$secondary[end_age_school$iso %in% ISO]-15+1)>5,5,(end_age_school$secondary[end_age_school$iso %in% ISO]-15+1))*
                                                         (enrolment_levels$secondary[enrolment_levels$iso %in% ISO]/100))
  }
  student_tertiary = 0
  
  if(start_age_school$tertiary[start_age_school$iso %in% ISO]>=15&start_age_school$tertiary[start_age_school$iso %in% ISO]<20) 
  {
    student_tertiary = student_tertiary + as.integer(pop['age15']/5*
                                                       (19-start_age_school$tertiary[start_age_school$iso %in% ISO]+1)*
                                                       (enrolment_levels$tertiary[enrolment_levels$iso %in% ISO]/100)) 
  }
  if(end_age_school$tertiary[end_age_school$iso %in% ISO]>19) 
  {
    student_tertiary = student_tertiary + as.integer(pop['age20']/5*
                                                       ifelse((end_age_school$tertiary[end_age_school$iso %in% ISO]-20+1)>5,5,(end_age_school$tertiary[end_age_school$iso %in% ISO]-20+1))*
                                                       (enrolment_levels$tertiary[enrolment_levels$iso %in% ISO]/100))
  }
  
  print(paste0("Pre-primary : ",student_preprimary))
  print(paste0("Primary : ",student_primary))
  print(paste0("Secondary : ",student_secondary))
  print(paste0("Tertiary : ",student_tertiary))
  
  teachers_bylevel[i,2] = as.integer(student_preprimary/(PTR_urban[PTR_urban$iso %in% ISO,2]))
  teachers_bylevel[i,3] = as.integer(student_primary/(PTR_urban[PTR_urban$iso %in% ISO,3]))
  teachers_bylevel[i,4] = as.integer(student_secondary/(PTR_urban[PTR_urban$iso %in% ISO,4]))
  teachers_bylevel[i,5] = as.integer(student_tertiary/(PTR_urban[PTR_urban$iso %in% ISO,5]))
}

teachers_urban = data.frame(iso = teachers_bylevel$iso,array(0,c(nrow(teachers_bylevel),16)))
colnames(teachers_urban)[2:ncol(teachers_urban)] = paste0('age',seq(0,75,5))

for(i in 1:nrow(teachers_urban))
{
  ISO = as.character(teachers_urban$iso[i])
  workindividuals = work[work$iso3c %in% ISO,6:21]*
    (popUrban$total_female[popUrban$total_female$iso3c %in%ISO,4:19]+popUrban$total_male[popUrban$total_male$iso3c %in%ISO,4:19])
  nteachers_age = list()
  for(j in 1:4) nteachers_age[[j]] = teachers_bylevel[teachers_bylevel$iso %in% ISO,j+1]*(proportion_teacheragelevel[[j]])*addpartime$factor[addpartime$iso %in% ISO]
  teachers_urban[i,6:7] = as.integer((nteachers_age[[1]][1]*workindividuals[5:6]/sum(workindividuals[5:6]))+
                                 (nteachers_age[[2]][1]*workindividuals[5:6]/sum(workindividuals[5:6]))+
                                 (nteachers_age[[3]][1]*workindividuals[5:6]/sum(workindividuals[5:6]))+
                                 (nteachers_age[[4]][1]*workindividuals[5:6]/sum(workindividuals[5:6])))
  teachers_urban[i,8:9] = as.integer((nteachers_age[[1]][2]*workindividuals[7:8]/sum(workindividuals[7:8]))+
                                 (nteachers_age[[2]][2]*workindividuals[7:8]/sum(workindividuals[7:8]))+
                                 (nteachers_age[[3]][2]*workindividuals[7:8]/sum(workindividuals[7:8]))+
                                 (nteachers_age[[4]][2]*workindividuals[7:8]/sum(workindividuals[7:8])))
  teachers_urban[i,10:11] = as.integer((nteachers_age[[1]][3]*workindividuals[9:10]/sum(workindividuals[9:10]))+
                                 (nteachers_age[[2]][3]*workindividuals[9:10]/sum(workindividuals[9:10]))+
                                 (nteachers_age[[3]][3]*workindividuals[9:10]/sum(workindividuals[9:10]))+
                                 (nteachers_age[[4]][3]*workindividuals[9:10]/sum(workindividuals[9:10])))
  teachers_urban[i,12:17] = as.integer((nteachers_age[[1]][4]*workindividuals[11:16]/sum(workindividuals[11:16]))+
                                   (nteachers_age[[2]][4]*workindividuals[11:16]/sum(workindividuals[11:16]))+
                                   (nteachers_age[[3]][4]*workindividuals[11:16]/sum(workindividuals[11:16]))+
                                   (nteachers_age[[4]][4]*workindividuals[11:16]/sum(workindividuals[11:16])))
  # teachers_urban[i,2:17] = as.integer(teachers_bylevel$preprimary[teachers_bylevel$iso %in% ISO]*(workindividuals/sum(workindividuals)))
}

save(teachers_urban,file = 'input/school/teachers_urban.rdata')


load('input/school/students_rural.rdata')
load('input/school/pupiltoteacherratio_rural.rdata')
load('input/pop/popRural.rdata')
load('input/work/workpopage_rural.rdata')

work = workpopage_rural$total
work[is.na(work)] = 0
iso = students_rural$iso
iso = iso[iso %in% PTR_rural$iso]
teachers_bylevel = data.frame(iso = iso,preprimary =NA, primary = NA,secondary = NA,tertiary = NA)
for(i in 1:nrow(teachers_bylevel))
{
  ISO = as.character(teachers_bylevel$iso[i])
  pop = popRural$total_female[popRural$total_female$iso3c %in%ISO,4:19]+popRural$total_male[popRural$total_male$iso3c %in%ISO,4:19]
  student_preprimary = as.integer(pop['age0']/5*
                                    (4-start_age_school$preprimary[start_age_school$iso %in% ISO]+1)*
                                    (enrolment_levels$preprimary[enrolment_levels$iso %in% ISO]/100))
  if(end_age_school$preprimary[end_age_school$iso %in% ISO]>4) 
  {
    student_preprimary = student_preprimary + as.integer(pop['age5']/5*
                                                           (end_age_school$preprimary[end_age_school$iso %in% ISO]-5+1)*
                                                           (enrolment_levels$preprimary[enrolment_levels$iso %in% ISO]/100))
  }
  student_primary = 0
  if(start_age_school$primary[start_age_school$iso %in% ISO]>=5&start_age_school$primary[start_age_school$iso %in% ISO]<10) 
  {
    student_primary = student_primary + as.integer(pop['age5']/5*
                                                     (9-start_age_school$primary[start_age_school$iso %in% ISO]+1)*
                                                     (enrolment_levels$primary[enrolment_levels$iso %in% ISO]/100)) 
  }
  if(end_age_school$primary[end_age_school$iso %in% ISO]>9) 
  {
    student_primary = student_primary + as.integer(pop['age10']/5*
                                                     ifelse((end_age_school$primary[end_age_school$iso %in% ISO]-10+1)>5,5,(end_age_school$primary[end_age_school$iso %in% ISO]-10+1))*
                                                     (enrolment_levels$primary[enrolment_levels$iso %in% ISO]/100))
  }
  student_secondary = 0
  if(start_age_school$secondary[start_age_school$iso %in% ISO]>=10&start_age_school$secondary[start_age_school$iso %in% ISO]<15) 
  {
    student_secondary = student_secondary + as.integer(pop['age10']/5*
                                                         (14-start_age_school$secondary[start_age_school$iso %in% ISO]+1)*
                                                         (enrolment_levels$secondary[enrolment_levels$iso %in% ISO]/100)) 
  }
  if(end_age_school$secondary[end_age_school$iso %in% ISO]>14) 
  {
    student_secondary = student_secondary + as.integer(pop['age15']/5*
                                                         ifelse((end_age_school$secondary[end_age_school$iso %in% ISO]-15+1)>5,5,(end_age_school$secondary[end_age_school$iso %in% ISO]-15+1))*
                                                         (enrolment_levels$secondary[enrolment_levels$iso %in% ISO]/100))
  }
  student_tertiary = 0
  
  if(start_age_school$tertiary[start_age_school$iso %in% ISO]>=15&start_age_school$tertiary[start_age_school$iso %in% ISO]<20) 
  {
    student_tertiary = student_tertiary + as.integer(pop['age15']/5*
                                                       (19-start_age_school$tertiary[start_age_school$iso %in% ISO]+1)*
                                                       (enrolment_levels$tertiary[enrolment_levels$iso %in% ISO]/100)) 
  }
  if(end_age_school$tertiary[end_age_school$iso %in% ISO]>19) 
  {
    student_tertiary = student_tertiary + as.integer(pop['age20']/5*
                                                       ifelse((end_age_school$tertiary[end_age_school$iso %in% ISO]-20+1)>5,5,(end_age_school$tertiary[end_age_school$iso %in% ISO]-20+1))*
                                                       (enrolment_levels$tertiary[enrolment_levels$iso %in% ISO]/100))
  }
  
  print(paste0("Pre-primary : ",student_preprimary))
  print(paste0("Primary : ",student_primary))
  print(paste0("Secondary : ",student_secondary))
  print(paste0("Tertiary : ",student_tertiary))
  
  teachers_bylevel[i,2] = as.integer(student_preprimary/(PTR_rural[PTR_rural$iso %in% ISO,2]))
  teachers_bylevel[i,3] = as.integer(student_primary/(PTR_rural[PTR_rural$iso %in% ISO,3]))
  teachers_bylevel[i,4] = as.integer(student_secondary/(PTR_rural[PTR_rural$iso %in% ISO,4]))
  teachers_bylevel[i,5] = as.integer(student_tertiary/(PTR_rural[PTR_rural$iso %in% ISO,5]))
}


teachers_rural = data.frame(iso = teachers_bylevel$iso,array(0,c(nrow(teachers_bylevel),16)))
colnames(teachers_rural)[2:ncol(teachers_rural)] = paste0('age',seq(0,75,5))

for(i in 1:nrow(teachers_rural))
{
  ISO = as.character(teachers_rural$iso[i])
  workindividuals = work[work$iso3c %in% ISO,6:21]*
    (popRural$total_female[popRural$total_female$iso3c %in%ISO,4:19]+popRural$total_male[popRural$total_male$iso3c %in%ISO,4:19])
  nteachers_age = list()
  for(j in 1:4) nteachers_age[[j]] = teachers_bylevel[teachers_bylevel$iso %in% ISO,j+1]*(proportion_teacheragelevel[[j]])*addpartime$factor[addpartime$iso %in% ISO]
  teachers_rural[i,6:7] = as.integer((nteachers_age[[1]][1]*workindividuals[5:6]/sum(workindividuals[5:6]))+
                                       (nteachers_age[[2]][1]*workindividuals[5:6]/sum(workindividuals[5:6]))+
                                       (nteachers_age[[3]][1]*workindividuals[5:6]/sum(workindividuals[5:6]))+
                                       (nteachers_age[[4]][1]*workindividuals[5:6]/sum(workindividuals[5:6])))
  teachers_rural[i,8:9] = as.integer((nteachers_age[[1]][2]*workindividuals[7:8]/sum(workindividuals[7:8]))+
                                       (nteachers_age[[2]][2]*workindividuals[7:8]/sum(workindividuals[7:8]))+
                                       (nteachers_age[[3]][2]*workindividuals[7:8]/sum(workindividuals[7:8]))+
                                       (nteachers_age[[4]][2]*workindividuals[7:8]/sum(workindividuals[7:8])))
  teachers_rural[i,10:11] = as.integer((nteachers_age[[1]][3]*workindividuals[9:10]/sum(workindividuals[9:10]))+
                                         (nteachers_age[[2]][3]*workindividuals[9:10]/sum(workindividuals[9:10]))+
                                         (nteachers_age[[3]][3]*workindividuals[9:10]/sum(workindividuals[9:10]))+
                                         (nteachers_age[[4]][3]*workindividuals[9:10]/sum(workindividuals[9:10])))
  teachers_rural[i,12:17] = as.integer((nteachers_age[[1]][4]*workindividuals[11:16]/sum(workindividuals[11:16]))+
                                         (nteachers_age[[2]][4]*workindividuals[11:16]/sum(workindividuals[11:16]))+
                                         (nteachers_age[[3]][4]*workindividuals[11:16]/sum(workindividuals[11:16]))+
                                         (nteachers_age[[4]][4]*workindividuals[11:16]/sum(workindividuals[11:16])))
  # teachers_urban[i,2:17] = as.integer(teachers_bylevel$preprimary[teachers_bylevel$iso %in% ISO]*(workindividuals/sum(workindividuals)))
}

save(teachers_rural,file = 'input/school/teachers_rural.rdata')


