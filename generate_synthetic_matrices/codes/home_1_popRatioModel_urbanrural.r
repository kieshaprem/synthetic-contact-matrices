# Population Ratio for POLYMOD/DHS
library(countrycode)
load('output/hamdhs_urban.rdata')
load('output/hamdhs_rural.rdata')
load('output/hampolymod.rdata')
load('input/pop/popUrban.rdata')
load('input/pop/popRural.rdata')
source('codes/functions_processContactmatrices.r')

# Country code 
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS_URBAN)
countries = c(POLYMODcountries,DHScountries)



POPRATIO_POLYMODDHS_URBAN =POPRATIO_POLYMODDHS_RURAL = list()
HAM_POLYMODDHS_URBAN = c(HAM_POLYMOD[1:8],HAM_DHS_URBAN) 
HAM_POLYMODDHS_RURAL = c(HAM_POLYMOD[1:8],HAM_DHS_RURAL) 

for(COUNTRY in 1:length(countries)){
  # POP = as.numeric(popratio[popratio$iso3c %in% countries[COUNTRY],paste0('age',seq(0,80,5))][1,])
  POP = c(as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% countries[COUNTRY],paste0('age',seq(0,70,5))][1,]),
          sum(as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% countries[COUNTRY],paste0('age',seq(75,100,5))][1,])))+
    c(as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% countries[COUNTRY],paste0('age',seq(0,70,5))][1,]),
      sum(as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% countries[COUNTRY],paste0('age',seq(75,100,5))][1,])))
  POP = POP/sum(POP)
  HAM = HAM_POLYMODDHS_URBAN[[countries[COUNTRY]]]
  POPRATIO_POLYMODDHS_URBAN[[countries[COUNTRY]]] = getPopratio(HAM[1:16,1:16],POP)
  rm(POP, HAM)
}

for(COUNTRY in 1:length(countries)){
  # POP = as.numeric(popratio[popratio$iso3c %in% countries[COUNTRY],paste0('age',seq(0,80,5))][1,])
  POP = c(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% countries[COUNTRY],paste0('age',seq(0,70,5))][1,]),
          sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% countries[COUNTRY],paste0('age',seq(75,100,5))][1,])))+
    c(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% countries[COUNTRY],paste0('age',seq(0,70,5))][1,]),
      sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% countries[COUNTRY],paste0('age',seq(75,100,5))][1,])))
  POP = POP/sum(POP)
  HAM = HAM_POLYMODDHS_RURAL[[countries[COUNTRY]]]
  POPRATIO_POLYMODDHS_RURAL[[countries[COUNTRY]]] = getPopratio(HAM[1:16,1:16],POP)
  rm(POP, HAM)
}

save(POPRATIO_POLYMODDHS_URBAN,file = 'output/popratio_polymoddhs_urban.rdata')
save(POPRATIO_POLYMODDHS_RURAL,file = 'output/popratio_polymoddhs_rural.rdata')

# par(mfrow = c(4,5))
# for(i in 1:length(countries)) image(HAM_POLYMODDHS_URBAN[[i]], main =countrycode(names(HAM_POLYMODDHS_URBAN)[i],origin = 'iso3c',destination = 'country.name'))
# par(mfrow = c(4,5))
# for(i in 1:length(countries)) image(POPRATIO_POLYMODDHS_URBAN[[i]], main =countrycode(names(POPRATIO_POLYMODDHS_URBAN)[i],origin = 'iso3c',destination = 'country.name'))
# par(mfrow = c(4,5))
# for(i in 1:length(countries)) image(POPRATIO_POLYMODDHS_RURAL[[i]], main =countrycode(names(POPRATIO_POLYMODDHS_RURAL)[i],origin = 'iso3c',destination = 'country.name'))

# par(mfrow = c(4,5))
# for(i in 1:length(countries)) image(POPRATIO_POLYMODDHS[[i]], main =countrycode(names(POPRATIO_POLYMODDHS_RURAL)[i],origin = 'iso3c',destination = 'country.name'))
