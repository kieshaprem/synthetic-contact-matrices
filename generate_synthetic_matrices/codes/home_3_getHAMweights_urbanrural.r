library(countrycode)
load('output/HAMweightsbootstrap.rdata')
load('output/hamdhs_urban.rdata')
load('output/hamdhs_rural.rdata')
load('output/hampolymod.rdata')
load('input/pop/popUrban.rdata')
load('input/pop/popRural.rdata')
load('output/popratio_polymoddhs_urban.rdata')
load('output/popratio_polymoddhs_rural.rdata')
source('codes/functions_processContactmatrices.r')

# Country codes 
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS_URBAN)
POLYMODandDHscountries = c(POLYMODcountries,DHScountries)
countries = names(wbootstrap)[(names(wbootstrap) %in% popUrban$total_female$iso3c)]

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


CORRELATION_pi_pa_urban = CORRELATION_pi_pa_rural = array(NA, c(nrow(wbootstrap[[1]]),length(countries))) 

for(co in 1:length(countries)){
  COUNTRY = countries[co] 
  wsamples = wbootstrap[[COUNTRY]]
  wsamples = wsamples[,colnames(wsamples)[!(colnames(wsamples) %in% COUNTRY)]]
  wsamples = 1/wsamples
  
  for(iteration in 1:nrow(wbootstrap[[1]]))
  {
    weights = wsamples[iteration,]
    weights = wsamples[iteration,]/sum(wsamples[iteration,])
    i=1;weightedsumpopratio = POPRATIO_POLYMODDHS_URBAN[[names(weights)[i]]] * weights[i]
    
    for(i in 2:length(weights)){ weightedsumpopratio = weightedsumpopratio + POPRATIO_POLYMODDHS_URBAN[[names(weights)[i]]] * weights[i]}
    
    HAM  = estimateHH(weightedsumpopratio,popage_urban[[COUNTRY]])
    piage = rowSums(popage_urban[[COUNTRY]] * HAM)
    CORRELATION_pi_pa_urban[iteration,co] = cor(piage,popage_urban[[COUNTRY]])
  }
  
  print(co)
}



for(co in 1:length(countries)){
  COUNTRY = countries[co] 
  wsamples = wbootstrap[[COUNTRY]]
  wsamples = wsamples[,colnames(wsamples)[!(colnames(wsamples) %in% COUNTRY)]]
  wsamples = 1/wsamples
  
  for(iteration in 1:nrow(wbootstrap[[1]]))
  {
    weights = wsamples[iteration,]
    weights = wsamples[iteration,]/sum(wsamples[iteration,])
    i=1;weightedsumpopratio = POPRATIO_POLYMODDHS_RURAL[[names(weights)[i]]] * weights[i]
    
    for(i in 2:length(weights)){ weightedsumpopratio = weightedsumpopratio + POPRATIO_POLYMODDHS_RURAL[[names(weights)[i]]] * weights[i]}
    
    HAM  = estimateHH(weightedsumpopratio,popage_rural[[COUNTRY]])
    piage = rowSums(popage_rural[[COUNTRY]] * HAM)
    CORRELATION_pi_pa_rural[iteration,co] = cor(piage,popage_rural[[COUNTRY]])
  }
  
  print(co)
}

head(CORRELATION_pi_pa_rural)
colnames(CORRELATION_pi_pa_urban) = colnames(CORRELATION_pi_pa_rural) = countries


maxcorr = apply(CORRELATION_pi_pa_urban,2,which.max)
HAMweights_urban = list()

for(co in 1:length(countries))
{
  COUNTRY = countries[co] 
  wsamples = wbootstrap[[COUNTRY]]
  wsamples = wsamples[,colnames(wsamples)[!(colnames(wsamples) %in% COUNTRY)]]
  wsamples = 1/wsamples
  
  for(iteration in 1:nrow(wbootstrap[[1]])) wsamples[iteration,]=wsamples[iteration,]/sum(wsamples[iteration,])
  
  HAMweights_urban[[COUNTRY]] = wsamples[maxcorr[COUNTRY],]
  print(co)
}
# lapply(HAMweights, function(x) which.max(x))

save(HAMweights_urban,file = 'output/HAMweights_urban.rdata')

maxcorr = unlist(apply(CORRELATION_pi_pa_rural,2,which.max))
HAMweights_rural = list()

for(co in 1:length(countries))
{
  COUNTRY = countries[co] 
  wsamples = wbootstrap[[COUNTRY]]
  wsamples = wsamples[,colnames(wsamples)[!(colnames(wsamples) %in% COUNTRY)]]
  wsamples = 1/wsamples
  
  for(iteration in 1:nrow(wbootstrap[[1]])) wsamples[iteration,]=wsamples[iteration,]/sum(wsamples[iteration,])
  
  HAMweights_rural[[COUNTRY]] = wsamples[maxcorr[COUNTRY],]
  print(co)
}
# lapply(HAMweights, function(x) which.max(x))

save(HAMweights_rural,file = 'output/HAMweights_rural.rdata')
