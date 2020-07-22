
load('output/hamdhs.rdata')
load('output/hampolymod.rdata')
load('output/popratio_polymoddhs.rdata')
load('input/pop/popratio.rdata')
load('output/HAMweights.rdata')
source('codes/functions_processContactmatrices.r')


# Country codes 
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS)
POLYMODandDHscountries = c(POLYMODcountries,DHScountries)
countries = as.character(sort(names(HAMweights)))
 

popage = list()
for(co in 1:length(countries)){
  popage[[countries[co]]] = as.numeric(popratio[popratio$iso3c %in% countries[co],paste0('age',seq(0,75,5))][1,])
}
rm(popratio,co)


HAM_WORLD = list()
for(co in 1:length(countries)) 
{
  HAM_WORLD[[countries[co]]] = getHAMWORLD(COUNTRY=countries[co])[['HAM']]
}
save(HAM_WORLD,file = 'output/hamworld.rdata')

HAM_POLYMODandDHS = c(HAM_POLYMOD[-9],HAM_DHS)
save(HAM_POLYMODandDHS,file = 'output/hampolymoddhs.rdata')

# i=20
# par(mfrow=c(1,2))
# image(HAM_DHS[[DHScountries[i]]])
# image(HAM_WORLD[[DHScountries[i]]])
# i=8
# par(mfrow=c(1,2))
# image(HAM_POLYMOD[[POLYMODcountries[i]]])
# image(HAM_WORLD[[POLYMODcountries[i]]])


