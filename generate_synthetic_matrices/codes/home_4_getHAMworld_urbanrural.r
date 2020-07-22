load('output/hamdhs_urban.rdata')
load('output/hamdhs_rural.rdata')
load('output/hampolymod.rdata')
load('output/popratio_polymoddhs_rural.rdata')
load('output/popratio_polymoddhs_urban.rdata')
load('input/pop/popUrban.rdata')
load('input/pop/popRural.rdata')
load('output/HAMweights_urban.rdata')
load('output/HAMweights_rural.rdata')
source('codes/functions_processContactmatrices.r')


# Country codes 
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS_RURAL)
POLYMODandDHscountries = c(POLYMODcountries,DHScountries)
countries = as.character(sort(names(HAMweights_urban)))
 

popage_urban = popage_rural = list()
for(co in 1:length(countries)){
  pop = as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% countries[co],paste0('age',seq(0,75,5))][1,])+
    as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% countries[co],paste0('age',seq(0,75,5))][1,])
  popage_urban[[countries[co]]] = pop/sum(pop)
  pop = as.numeric(popRural$total_female[popRural$total_female$iso3c %in% countries[co],paste0('age',seq(0,75,5))][1,])+
    as.numeric(popRural$total_male[popRural$total_male$iso3c %in% countries[co],paste0('age',seq(0,75,5))][1,])
  popage_rural[[countries[co]]] = pop/sum(pop)
}
rm(popRural,popUrban,co,pop)

HAM_WORLD_URBAN = HAM_WORLD_RURAL = list()
for(co in 1:length(countries)) 
{
  HAM_WORLD_URBAN[[countries[co]]] = getHAMWORLD_URBANRURAL(COUNTRY=countries[co],
                                                            HAMweights = HAMweights_urban,
                                                            POPRATIO_POLYMODDHS = POPRATIO_POLYMODDHS_URBAN,
                                                            popage = popage_urban)[['HAM']]
  HAM_WORLD_RURAL[[countries[co]]] = getHAMWORLD_URBANRURAL(COUNTRY=countries[co],
                                                            HAMweights = HAMweights_rural,
                                                            POPRATIO_POLYMODDHS = POPRATIO_POLYMODDHS_RURAL,
                                                            popage = popage_rural)[['HAM']]
}
save(HAM_WORLD_URBAN,file = 'output/hamworld_urban.rdata')
save(HAM_WORLD_RURAL,file = 'output/hamworld_rural.rdata')
# HAM_POLYMODandDHS = c(HAM_POLYMOD[-9],HAM_DHS)
# save(HAM_POLYMODandDHS,file = 'output/hampolymoddhs.rdata')

# i=20
# par(mfrow=c(1,2))
# image(HAM_DHS[[DHScountries[i]]])
# image(HAM_WORLD[[DHScountries[i]]])
# i=8
# par(mfrow=c(1,2))
# image(HAM_POLYMOD[[POLYMODcountries[i]]])
# image(HAM_WORLD[[POLYMODcountries[i]]])


