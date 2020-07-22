# Population Ratio for POLYMOD/DHS
library(countrycode)
load('output/hamdhs.rdata')
load('output/hampolymod.rdata')
load('input/pop/popratio.rdata')
load('input/pop/poptotal.rdata')
source('codes/functions_processContactmatrices.r')

# Country code 
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS)
countries = c(POLYMODcountries,DHScountries)



POPRATIO_POLYMODDHS = list()
HAM_POLYMODDHS = c(HAM_POLYMOD[1:8],HAM_DHS) 


for(COUNTRY in 1:length(countries)){
  # POP = as.numeric(popratio[popratio$iso3c %in% countries[COUNTRY],paste0('age',seq(0,80,5))][1,])
  POP = c(as.numeric(popratio[popratio$iso3c %in% countries[COUNTRY],paste0('age',seq(0,70,5))][1,]),
          sum(as.numeric(popratio[popratio$iso3c %in% countries[COUNTRY],paste0('age',seq(75,100,5))][1,])))
  HAM = HAM_POLYMODDHS[[countries[COUNTRY]]]
  POPRATIO_POLYMODDHS[[countries[COUNTRY]]] = getPopratio(HAM[1:16,1:16],POP)
  rm(POP, HAM)
}


save(POPRATIO_POLYMODDHS,file = 'output/popratio_polymoddhs.rdata')

# par(mfrow = c(4,5))
# for(i in 1:length(countries)) image(HAM_POLYMODDHS[[i]], main =countrycode(names(HAM_POLYMODDHS)[i],origin = 'iso3c',destination = 'country.name'))
# par(mfrow = c(4,5))
# for(i in 1:length(countries)) image(POPRATIO_POLYMODDHS[[i]], main =countrycode(names(POPRATIO_POLYMODDHS)[i],origin = 'iso3c',destination = 'country.name'))

