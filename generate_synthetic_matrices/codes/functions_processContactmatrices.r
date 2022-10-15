
getLatestData_indicator = function(ALLDATA){
  TRIMDATA = list()
  TRIMDATA$iso3c = sort(unique(ALLDATA$iso3c))
  TRIMDATA$year = TRIMDATA$value = TRIMDATA$indicator = TRIMDATA$country =  array(NA, length(TRIMDATA$iso3c))
  
  for(iso in 1:length(TRIMDATA$iso3c))
  {
    COUNTRYDATA= ALLDATA[which(ALLDATA$iso3c %in% TRIMDATA$iso3c[iso]),]
    j = which.max(COUNTRYDATA$date)
    TRIMDATA$year[iso] = as.integer(COUNTRYDATA$date[j])
    TRIMDATA$value[iso] = as.numeric(COUNTRYDATA$value[j])
    TRIMDATA$indicator[iso] = (COUNTRYDATA$indicator[j])
    TRIMDATA$country[iso] = as.character(COUNTRYDATA$country[j])
  }
  TRIMDATA = do.call(cbind.data.frame, TRIMDATA)
  TRIMDATA = TRIMDATA[order(TRIMDATA$iso3c),]
  return(TRIMDATA)
}

# functions to substitute missing values of Pupil to Teacher ratio and enrolment rates with the regional value
getRegionalMean = function(isocheck,DATA){
  mean(DATA$value[DATA$iso3c %in% as.character(work$iso3c[work$subregion %in% work$subregion[work$iso3c %in% isocheck]])],na.rm = TRUE)
}

getPopratio = function(HAM,pop){
  t(t(HAM[1:16,1:16])/pop[1:16])
}

estimateHH = function(popratio,pop){
  t(t(popratio[1:16,1:16])*pop[1:16])
}

getHAMWORLD = function(COUNTRY){
  finalweights = HAMweights[[COUNTRY]]
  i=1;weightedsumpopratio = POPRATIO_POLYMODDHS[[names(finalweights)[i]]] * finalweights[i]
  for(i in 2:length(finalweights)){ weightedsumpopratio = weightedsumpopratio + POPRATIO_POLYMODDHS[[names(finalweights)[i]]] * finalweights[i]}
  HAM = estimateHH(weightedsumpopratio,popage[[COUNTRY]])
  piagevalidated = rowSums(popage[[COUNTRY]] * HAM)
  correlation_pi_pa = (cor(piagevalidated,popage[[COUNTRY]]))
  output = list(HAM,popage[[COUNTRY]],piagevalidated,correlation_pi_pa)
  names(output) = c('HAM','popage','piage','correlation_pi_pa')
  return(output)
}

getHAMWORLD_URBANRURAL = function(COUNTRY,HAMweights,POPRATIO_POLYMODDHS,popage){
  finalweights = HAMweights[[COUNTRY]]
  i=1;weightedsumpopratio = POPRATIO_POLYMODDHS[[names(finalweights)[i]]] * finalweights[i]
  for(i in 2:length(finalweights)){ weightedsumpopratio = weightedsumpopratio + POPRATIO_POLYMODDHS[[names(finalweights)[i]]] * finalweights[i]}
  HAM = estimateHH(weightedsumpopratio,popage[[COUNTRY]])
  piagevalidated = rowSums(popage[[COUNTRY]] * HAM)
  correlation_pi_pa = (cor(piagevalidated,popage[[COUNTRY]]))
  output = list(HAM,popage[[COUNTRY]],piagevalidated,correlation_pi_pa)
  names(output) = c('HAM','popage','piage','correlation_pi_pa')
  return(output)
}

inferred.hhcontact = function(HH,AGEMAX){
  data.matrix(lambdas$POLYMOD$home[1:AGEMAX,1:AGEMAX]*(HH[1:AGEMAX,1:AGEMAX])+deltas$POLYMOD$home)
}

HHcontact_POLYMOD_DHS = function(COUNTRY,POLYMOD){
  HH = HAM_POLYMODandDHS[[COUNTRY]]
  if(POLYMOD==FALSE) contact_home = inferred.hhcontact(HH = HH,AGEMAX = 16)
  if(POLYMOD==TRUE) contact_home = data.matrix(KAPPA_HOME[[COUNTRY]][1:16,1:16]*(HH[1:16,1:16])+deltas[[COUNTRY]])
  return(contact_home)
}

HHcontact_POLYMOD_DHS = function(COUNTRY,POLYMOD){
  HH = HAM_POLYMODandDHS[[COUNTRY]]
  if(POLYMOD==FALSE) contact_home = inferred.hhcontact(HH = HH,AGEMAX = 16)
  if(POLYMOD==TRUE) contact_home = data.matrix(lambdas[[COUNTRY]]$home[1:16,1:16]*(HH[1:16,1:16])+deltas[[COUNTRY]]$home)
  return(contact_home)
}

HHcontact_Rest_of_world = function(COUNTRY){
  HH = HAM_WORLD[[(COUNTRY)]]
  contact_home = inferred.hhcontact(HH = HH,AGEMAX = 16)
  return(contact_home)
}

getWorkPop = function(COUNTRY,POLYMOD,SURVEYPROP = polymod_pworkandsch){
  workingpop = as.numeric(work[work$iso3c %in% COUNTRY,6:21])
  workingpop[1:3] =0
  workingpop[15:16]=0.01
  
  POLYMODcountries = c("BEL", "DEU","FIN", "GBR" ,"ITA" ,"LUX" ,"NLD","POL")
  polymodwork = colMeans(work[work$iso3c %in% POLYMODcountries,6:21],na.rm=TRUE)
  workingratio = workingpop/(polymodwork)
  
  if(POLYMOD==TRUE) wa = SURVEYPROP[[COUNTRY]]$pwork 
  if(POLYMOD==FALSE)  wa = SURVEYPROP$POLYMOD$pwork * (workingpop)
  
  return(wa)
}

getSchoolPop = function(COUNTRY,POLYMOD,SURVEYPROP = polymod_pworkandsch){
  schoolpop = as.numeric(school[school$iso %in% COUNTRY,grep(pattern = 'age',x = colnames(school))])
  
  POLYMODcountries = c("BEL", "DEU","FIN", "GBR" ,"ITA" ,"LUX" ,"NLD","POL")
  polymodsch = colMeans(school[school$iso %in% POLYMODcountries,grep(pattern = 'age',x = colnames(school))],na.rm=TRUE)
  
  if(POLYMOD==TRUE) sa =  c(SURVEYPROP$POLYMOD$psch/schoolpop[1:4],schoolpop[5:16]) 
  if(POLYMOD==FALSE)  sa =  c(SURVEYPROP$POLYMOD$psch/polymodsch[1:4],polymodsch[5:16])*schoolpop
  sa[is.na(sa)] = 0
  return(sa)
}