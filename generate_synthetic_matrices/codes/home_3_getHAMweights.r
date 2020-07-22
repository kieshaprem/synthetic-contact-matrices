library(countrycode)
load('output/HAMweightsbootstrap.rdata')
load('output/hamdhs.rdata')
load('output/hampolymod.rdata')
load('output/popratio_polymoddhs.rdata')
load('input/pop/popratio.rdata')
source('codes/functions_processContactmatrices.r')

# Country codes 
POLYMODcountries = names(HAM_POLYMOD)[-9]
DHScountries = names(HAM_DHS)
POLYMODandDHscountries = c(POLYMODcountries,DHScountries)
countries = names(wbootstrap)[(names(wbootstrap) %in% popratio$iso3c)]

popage = list()
for(co in 1:length(countries)){
  popage[[countries[co]]] = as.numeric(popratio[popratio$iso3c %in% countries[co],paste0('age',seq(0,75,5))][1,])
}
rm(popratio,co)


CORRELATION_pi_pa = array(NA, c(nrow(wbootstrap[[1]]),length(countries))) 

for(co in 1:length(countries)){
  COUNTRY = countries[co] 
  wsamples = wbootstrap[[COUNTRY]]
  wsamples = wsamples[,colnames(wsamples)[!(colnames(wsamples) %in% COUNTRY)]]
  wsamples = 1/wsamples
  
  for(iteration in 1:nrow(wbootstrap[[1]]))
  {
    weights = wsamples[iteration,]
    weights = wsamples[iteration,]/sum(wsamples[iteration,])
    i=1;weightedsumpopratio = POPRATIO_POLYMODDHS[[names(weights)[i]]] * weights[i]
    
    for(i in 2:length(weights)){ weightedsumpopratio = weightedsumpopratio + POPRATIO_POLYMODDHS[[names(weights)[i]]] * weights[i]}
    
    HAM  = estimateHH(weightedsumpopratio,popage[[COUNTRY]])
    piage = rowSums(popage[[COUNTRY]] * HAM)
    CORRELATION_pi_pa[iteration,co] = cor(piage,popage[[COUNTRY]])
  }
  
  print(co)
}


head(CORRELATION_pi_pa)
colnames(CORRELATION_pi_pa) = countries
maxcorr = apply(CORRELATION_pi_pa,2,which.max)

HAMweights = list()

for(co in 1:length(countries))
{
  COUNTRY = countries[co] 
  wsamples = wbootstrap[[COUNTRY]]
  wsamples = wsamples[,colnames(wsamples)[!(colnames(wsamples) %in% COUNTRY)]]
  wsamples = 1/wsamples
  
  for(iteration in 1:nrow(wbootstrap[[1]])) wsamples[iteration,]=wsamples[iteration,]/sum(wsamples[iteration,])
  
  HAMweights[[COUNTRY]] = wsamples[maxcorr[COUNTRY],]
  print(co)
}
# lapply(HAMweights, function(x) which.max(x))

save(HAMweights,file = 'output/HAMweights.rdata')
