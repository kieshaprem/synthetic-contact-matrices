
##### Working population age structure

# To build the working population age structure,  
# the labour force participation rate by sex and 5-year age groups for most countries of the world 
# were obtained from the International Labor Organization on-line database: 
#
# ILO Labour Force Estimates and Projections by age, November 2019									
# The ILO Labour Force Estimates and Projections (LFEP) Database includes national, regional and global data with indicators including the labour force, 
# labour force participation rates and population for the years 1990 to 2030. The 2019 UN World Population Prospects population estimates and projections 
# serve as the population benchmark. This indicator is part of the ILO Estimates and Projections series. For more information, refer to the indicator description
# and the labour force estimates and projections methodological paper below.									
# Indicator description									
# Labour force estimates and projections methodological paper									
# https://ilostat.ilo.org/data/bulk/ 				
#
# Labour force participation rate by sex, age and rural / urban areas -- ILO modelled estimates, Nov. 2019 (%) -- Annual
# Collection: ILOEST -- ILO estimates
# Id: EAP_2WAP_SEX_AGE_GEO_RT_A
# Time period: 2005 - 2024
# Last updated: 26/03/2020 10:28:48
# Number of records: 149040

#### load data
labourforce = read.csv('input/work/labour_force_participation_rate_ILO_modelled_estimates.csv', stringsAsFactors = FALSE)
lf_urbanrural = read.csv('input/work/ilostat_ubranrural.csv', stringsAsFactors = FALSE)
							

#### create working population age structure (total, male, female)
list_countrycodes = sort(unique(labourforce$iso3c))
list_countrycodes = list_countrycodes[!(list_countrycodes %in% "")]

lf_urbanrural2020  = lf_urbanrural[lf_urbanrural$time %in% 2020,]
rm(lf_urbanrural)
lf_urbanrural2020$iso3c = countrycode::countrycode(sourcevar = lf_urbanrural2020$ref_area.label,origin = 'country.name',destination = 'iso3c')
lf_urbanrural2020$iso3c[lf_urbanrural2020$ref_area.label %in%  "Channel Islands"] = "CHA"
sort(unique(lf_urbanrural2020$iso3c)) %in% list_countrycodes
list_countrycodes[!(list_countrycodes %in% sort(unique(lf_urbanrural2020$iso3c)))]

workpopage = list()
workpopage$total = data.frame(iso3c=list_countrycodes,subregion = NA, 
                         incomegroup = NA,year = 2020,sex = 'total', 
                         age0 = NA, age5 = NA,
                         age10 = NA, age15 = NA,
                         age20 = NA, age25 = NA,
                         age30 = NA, age35 = NA,
                         age40 = NA, age45 = NA,
                         age50 = NA, age55 = NA,
                         age60 = NA, age65 = NA,
                         age70 = NA, age75 = NA)

workpopage$female = data.frame(iso3c=list_countrycodes,subregion = NA, 
                                              incomegroup = NA,year = 2020,sex = 'female', 
                                              age0 = NA, age5 = NA,
                                              age10 = NA, age15 = NA,
                                              age20 = NA, age25 = NA,
                                              age30 = NA, age35 = NA,
                                              age40 = NA, age45 = NA,
                                              age50 = NA, age55 = NA,
                                              age60 = NA, age65 = NA,
                                              age70 = NA, age75 = NA)

workpopage$male  = data.frame(iso3c=list_countrycodes,subregion = NA, 
                                            incomegroup = NA,year = 2020,sex = 'male', 
                                            age0 = NA, age5 = NA,
                                            age10 = NA, age15 = NA,
                                            age20 = NA, age25 = NA,
                                            age30 = NA, age35 = NA,
                                            age40 = NA, age45 = NA,
                                            age50 = NA, age55 = NA,
                                            age60 = NA, age65 = NA,
                                            age70 = NA, age75 = NA)
rm(list_countrycodes)
workpopage_urban = workpopage_rural = workpopage
rm(workpopage)
for(i in 1:nrow(workpopage_urban$total))
{
  if(sum(labourforce$iso3c %in% workpopage_urban$total$iso3c[i],na.rm = TRUE)>0)
  {
    index = which(labourforce$iso3c %in% workpopage_urban$total$iso3c[i])
    index_u = which(lf_urbanrural2020$iso3c %in% workpopage_urban$total$iso3c[i] & 
                     lf_urbanrural2020$sex.label %in% "Sex: Total" &
                     lf_urbanrural2020$classif2.label %in% "Area type: Urban")
    index_n = which(lf_urbanrural2020$iso3c %in% workpopage_urban$total$iso3c[i] & 
                          lf_urbanrural2020$sex.label %in% "Sex: Total" &
                          lf_urbanrural2020$classif2.label %in% "Area type: National")
    
    workpopage_urban$total$subregion[i]= labourforce$subregion[index][1]
    workpopage_urban$total$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage_urban$total[i,6:19] = labourforce$total[index]/100
    workpopage_urban$total[i,9:10] = workpopage_urban$total[i,9:10]*(lf_urbanrural2020$obs_value[index_u[2]])/(lf_urbanrural2020$obs_value[index_n[2]])
    workpopage_urban$total[i,11:19] = workpopage_urban$total[i,11:19]*(lf_urbanrural2020$obs_value[index_u[3]])/(lf_urbanrural2020$obs_value[index_n[3]])
   

    index_u = which(lf_urbanrural2020$iso3c %in% workpopage_urban$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Female" &
                      lf_urbanrural2020$classif2.label %in% "Area type: Urban")
    index_n = which(lf_urbanrural2020$iso3c %in% workpopage_urban$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Female" &
                      lf_urbanrural2020$classif2.label %in% "Area type: National")
    
    workpopage_urban$female$subregion[i]= labourforce$subregion[index][1]
    workpopage_urban$female$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage_urban$female[i,6:19] = labourforce$female[index]/100
    workpopage_urban$female[i,9:10] = workpopage_urban$female[i,9:10]*(lf_urbanrural2020$obs_value[index_u[2]])/(lf_urbanrural2020$obs_value[index_n[2]])
    workpopage_urban$female[i,11:19] = workpopage_urban$female[i,11:19]*(lf_urbanrural2020$obs_value[index_u[3]])/(lf_urbanrural2020$obs_value[index_n[3]])
    
    

    index_u = which(lf_urbanrural2020$iso3c %in% workpopage_urban$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Male" &
                      lf_urbanrural2020$classif2.label %in% "Area type: Urban")
    index_n = which(lf_urbanrural2020$iso3c %in% workpopage_urban$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Male" &
                      lf_urbanrural2020$classif2.label %in% "Area type: National")
    
    workpopage_urban$male$subregion[i]= labourforce$subregion[index][1]
    workpopage_urban$male$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage_urban$male[i,6:19] = labourforce$male[index]/100
    workpopage_urban$male[i,9:10] = workpopage_urban$male[i,9:10]*(lf_urbanrural2020$obs_value[index_u[2]])/(lf_urbanrural2020$obs_value[index_n[2]])
    workpopage_urban$male[i,11:19] = workpopage_urban$male[i,11:19]*(lf_urbanrural2020$obs_value[index_u[3]])/(lf_urbanrural2020$obs_value[index_n[3]])
    

  }
}

for(i in 1:nrow(workpopage_rural$total))
{
  if(sum(labourforce$iso3c %in% workpopage_rural$total$iso3c[i],na.rm = TRUE)>0)
  {
    index = which(labourforce$iso3c %in% workpopage_rural$total$iso3c[i])
    index_u = which(lf_urbanrural2020$iso3c %in% workpopage_rural$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Total" &
                      lf_urbanrural2020$classif2.label %in% "Area type: Rural")
    index_n = which(lf_urbanrural2020$iso3c %in% workpopage_rural$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Total" &
                      lf_urbanrural2020$classif2.label %in% "Area type: National")
    
    workpopage_rural$total$subregion[i]= labourforce$subregion[index][1]
    workpopage_rural$total$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage_rural$total[i,6:19] = labourforce$total[index]/100
    workpopage_rural$total[i,9:10] = workpopage_rural$total[i,9:10]*(lf_urbanrural2020$obs_value[index_u[2]])/(lf_urbanrural2020$obs_value[index_n[2]])
    workpopage_rural$total[i,11:19] = workpopage_rural$total[i,11:19]*(lf_urbanrural2020$obs_value[index_u[3]])/(lf_urbanrural2020$obs_value[index_n[3]])
    
    
    index_u = which(lf_urbanrural2020$iso3c %in% workpopage_rural$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Female" &
                      lf_urbanrural2020$classif2.label %in% "Area type: Rural")
    index_n = which(lf_urbanrural2020$iso3c %in% workpopage_rural$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Female" &
                      lf_urbanrural2020$classif2.label %in% "Area type: National")
    
    workpopage_rural$female$subregion[i]= labourforce$subregion[index][1]
    workpopage_rural$female$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage_rural$female[i,6:19] = labourforce$female[index]/100
    workpopage_rural$female[i,9:10] = workpopage_rural$female[i,9:10]*(lf_urbanrural2020$obs_value[index_u[2]])/(lf_urbanrural2020$obs_value[index_n[2]])
    workpopage_rural$female[i,11:19] = workpopage_rural$female[i,11:19]*(lf_urbanrural2020$obs_value[index_u[3]])/(lf_urbanrural2020$obs_value[index_n[3]])
    
    
    
    index_u = which(lf_urbanrural2020$iso3c %in% workpopage_rural$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Male" &
                      lf_urbanrural2020$classif2.label %in% "Area type: Rural")
    index_n = which(lf_urbanrural2020$iso3c %in% workpopage_rural$total$iso3c[i] & 
                      lf_urbanrural2020$sex.label %in% "Sex: Male" &
                      lf_urbanrural2020$classif2.label %in% "Area type: National")
    
    workpopage_rural$male$subregion[i]= labourforce$subregion[index][1]
    workpopage_rural$male$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage_rural$male[i,6:19] = labourforce$male[index]/100
    workpopage_rural$male[i,9:10] = workpopage_rural$male[i,9:10]*(lf_urbanrural2020$obs_value[index_u[2]])/(lf_urbanrural2020$obs_value[index_n[2]])
    workpopage_rural$male[i,11:19] = workpopage_rural$male[i,11:19]*(lf_urbanrural2020$obs_value[index_u[3]])/(lf_urbanrural2020$obs_value[index_n[3]])
    
    
  }
}
rm(i,index,index_n,index_u,lf_urbanrural2020,labourforce)

save(workpopage_urban,file = 'input/work/workpopage_urban.rdata')
save(workpopage_rural,file = 'input/work/workpopage_rural.rdata')
rm(workpopage_rural,workpopage_urban)
