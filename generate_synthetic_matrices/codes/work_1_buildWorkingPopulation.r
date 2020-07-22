
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


#### load data
labourforce = read.csv('input/work/labour_force_participation_rate_ILO_modelled_estimates.csv', as.is = TRUE)

							

#### create working population age structure (total, male, female)

list_countrycodes = sort(unique(labourforce$iso3c))
list_countrycodes = list_countrycodes[!(list_countrycodes %in% "")]

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

for(i in 1:nrow(workpopage$total))
{
  if(sum(labourforce$iso3c %in% workpopage$total$iso3c[i],na.rm = TRUE)>0)
  {
    index = which(labourforce$iso3c %in% workpopage$total$iso3c[i])
    
    workpopage$total$subregion[i]= labourforce$subregion[index][1]
    workpopage$total$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage$total[i,6:19] = labourforce$total[index]/100
    
    workpopage$female$subregion[i]= labourforce$subregion[index][1]
    workpopage$female$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage$female[i,6:19] = labourforce$female[index]/100
    
    workpopage$male$subregion[i]= labourforce$subregion[index][1]
    workpopage$male$incomegroup[i]= gsub(pattern = "World: ",replacement = "",x = labourforce$income.group[index][1])
    workpopage$male[i,6:19] = labourforce$male[index]/100
  }
}

rm(i,index,labourforce)

save(workpopage,file = 'input/work/workpopage.rdata')

rm(workpopage)