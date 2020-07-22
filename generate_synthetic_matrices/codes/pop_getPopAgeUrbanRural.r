## POPULATION AGE STRUCTURE BY RURAL/URBAN: Urban and Rural Population by Age and Sex, 1980-2015
# United Nations, Department of Economic and Social Affairs, Population Division (2014). 
# https://www.un.org/en/development/desa/population/publications/dataset/urban/urbanAndRuralPopulationByAgeAndSex.asp

library(countrycode)
options(scipen=999)
popurbanrural = read.csv('input/pop/popage_ruralurban2015.csv', stringsAsFactors = FALSE)

popurbanrural$iso3c = (countrycode(sourcevar = popurbanrural$LocationName,origin = 'country.name',destination = 'iso3c'))
countries = unique(sort(popurbanrural$iso3c))
popurbanrural$AreaType %in% "Rural"
# popurbanrural[popurbanrural$iso3c %in% countries[co],]

urbanicity = list()
urbanicity$female = urbanicity$male = data.frame(iso3c = countries, total = NA,array(NA,c(length(countries),21)))
colnames(urbanicity$female)[3:23] = paste0('age',seq(0,100,5))
colnames(urbanicity$male)[3:23] = paste0('age',seq(0,100,5))

for(co in 1:nrow(urbanicity$female))
{
  ISO = urbanicity$female$iso3c[co]
  urbanicity$female[urbanicity$female$iso3c %in% ISO,2:19] = as.numeric(popurbanrural[(popurbanrural$iso3c %in% ISO)&(popurbanrural$Sex %in% "Female")&(popurbanrural$AreaType %in% "Urban"),5:22]/
                                                                          popurbanrural[(popurbanrural$iso3c %in% ISO)&(popurbanrural$Sex %in% "Female")&(popurbanrural$AreaType %in% "Total"),5:22])
  urbanicity$male[urbanicity$male$iso3c %in% ISO,2:19] = as.numeric(popurbanrural[(popurbanrural$iso3c %in% ISO)&(popurbanrural$Sex %in% "Male")&(popurbanrural$AreaType %in% "Urban"),5:22]/
                                                                          popurbanrural[(popurbanrural$iso3c %in% ISO)&(popurbanrural$Sex %in% "Male")&(popurbanrural$AreaType %in% "Total"),5:22])
}
urbanicity$female$age85 = urbanicity$female$age80
urbanicity$female$age90 = urbanicity$female$age80
urbanicity$female$age95 = urbanicity$female$age80
urbanicity$female$age100 = urbanicity$female$age80 

urbanicity$male$age85 = urbanicity$male$age80
urbanicity$male$age90 = urbanicity$male$age80
urbanicity$male$age95 = urbanicity$male$age80
urbanicity$male$age100 = urbanicity$male$age80 

load('input/pop/popfemale.rdata')
load('input/pop/popmale.rdata')

popUrban = popRural = list()

popUrban$total_female = popfemale
popUrban$total_female[,4:25] = NA
popUrban$total_female = popUrban$total_female[!is.na(popUrban$total_female$iso3c),]
popUrban$total_male = popUrban$total_female
popUrban$ratio_female = popUrban$total_female
popUrban$ratio_male = popUrban$total_female

popRural$total_female = popUrban$total_female 
popRural$total_male = popUrban$total_female 
popRural$ratio_female = popUrban$total_female 
popRural$ratio_male = popUrban$total_female 

for(co in 1:nrow(popUrban$total_female))
{
  ISO = popUrban$total_female$iso3c[co]
  
  # female urban
  popUrban$total_female[popUrban$total_female$iso3c %in% ISO,4:24] = (popfemale[popfemale$iso3c %in% ISO,4:24])*(urbanicity$female[urbanicity$female$iso3c %in% ISO,3:23])
  popUrban$total_female$total[popUrban$total_female$iso3c %in% ISO] = (popfemale$total[popfemale$iso3c %in% ISO])*(urbanicity$female$total[urbanicity$female$iso3c %in% ISO])
  popUrban$ratio_female$total[popUrban$ratio_female$iso3c %in% ISO] = (popfemale$total[popfemale$iso3c %in% ISO])*(urbanicity$female$total[urbanicity$female$iso3c %in% ISO])
  
  
  # male urban
  popUrban$total_male[popUrban$total_male$iso3c %in% ISO,4:24] = (popmale[popmale$iso3c %in% ISO,4:24])*(urbanicity$male[urbanicity$male$iso3c %in% ISO,3:23])
  popUrban$total_male$total[popUrban$total_male$iso3c %in% ISO] = (popmale$total[popmale$iso3c %in% ISO])*(urbanicity$male$total[urbanicity$male$iso3c %in% ISO])
  popUrban$ratio_male$total[popUrban$ratio_male$iso3c %in% ISO] = (popmale$total[popmale$iso3c %in% ISO])*(urbanicity$male$total[urbanicity$male$iso3c %in% ISO])
  popUrban$ratio_male[popUrban$ratio_male$iso3c %in% ISO,4:24] = popUrban$total_male[popUrban$total_male$iso3c %in% ISO,4:24]/(popUrban$total_female$total[popUrban$total_female$iso3c %in% ISO]+popUrban$total_male$total[popUrban$total_male$iso3c %in% ISO])   
  
  popUrban$ratio_female[popUrban$ratio_female$iso3c %in% ISO,4:24] = popUrban$total_female[popUrban$total_female$iso3c %in% ISO,4:24]/(popUrban$total_female$total[popUrban$total_female$iso3c %in% ISO]+popUrban$total_male$total[popUrban$total_male$iso3c %in% ISO])  
  
  # female rural
  popRural$total_female[popRural$total_female$iso3c %in% ISO,4:24] = (popfemale[popfemale$iso3c %in% ISO,4:24])*(1-urbanicity$female[urbanicity$female$iso3c %in% ISO,3:23])
  popRural$total_female$total[popRural$total_female$iso3c %in% ISO] = (popfemale$total[popfemale$iso3c %in% ISO])*(1-urbanicity$female$total[urbanicity$female$iso3c %in% ISO])
  popRural$ratio_female$total[popRural$ratio_female$iso3c %in% ISO] = (popfemale$total[popfemale$iso3c %in% ISO])*(1-urbanicity$female$total[urbanicity$female$iso3c %in% ISO])
  
  # male rural
  popRural$total_male[popRural$total_male$iso3c %in% ISO,4:24] = (popmale[popmale$iso3c %in% ISO,4:24])*(1-urbanicity$male[urbanicity$male$iso3c %in% ISO,3:23])
  popRural$total_male$total[popRural$total_male$iso3c %in% ISO] = (popmale$total[popmale$iso3c %in% ISO])*(1-urbanicity$male$total[urbanicity$male$iso3c %in% ISO])
  popRural$ratio_male$total[popRural$ratio_male$iso3c %in% ISO] = (popmale$total[popmale$iso3c %in% ISO])*(1-urbanicity$male$total[urbanicity$male$iso3c %in% ISO])
  if(popRural$total_male$total[popRural$total_male$iso3c %in% ISO]>0) popRural$ratio_male[popRural$ratio_male$iso3c %in% ISO,4:24] = popRural$total_male[popRural$total_male$iso3c %in% ISO,4:24]/(popRural$total_male$total[popRural$total_male$iso3c %in% ISO]+popRural$total_female$total[popRural$total_female$iso3c %in% ISO])  
  if(popRural$total_male$total[popRural$total_male$iso3c %in% ISO]==0) popRural$ratio_male[popRural$ratio_male$iso3c %in% ISO,4:24] = 0
  
  if(popRural$total_female$total[popRural$total_female$iso3c %in% ISO]>0) popRural$ratio_female[popRural$ratio_female$iso3c %in% ISO,4:24] = popRural$total_female[popRural$total_female$iso3c %in% ISO,4:24]/(popRural$total_male$total[popRural$total_male$iso3c %in% ISO]+popRural$total_female$total[popRural$total_female$iso3c %in% ISO])  
  if(popRural$total_female$total[popRural$total_female$iso3c %in% ISO]==0) popRural$ratio_female[popRural$ratio_female$iso3c %in% ISO,4:24] = 0
  
}

save(urbanicity,file = 'input/pop/urbanicity.rdata')
save(popUrban,file = 'input/pop/popUrban.rdata')
save(popRural,file = 'input/pop/popRural.rdata')
