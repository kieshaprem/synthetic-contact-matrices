
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
  apply(HAM[1:16,1:16],2,function(x) x/pop[1:16])
}

estimateHH = function(popratio,pop){
  apply(popratio,2,function(x) x*pop[1:16])
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
  data.matrix(KAPPA_HOME$POLYMOD[1:AGEMAX,1:AGEMAX]*(HH[1:AGEMAX,1:AGEMAX]))
}

HHcontact_POLYMOD_DHS = function(COUNTRY,POLYMOD){
  HH = HAM_POLYMODandDHS[[COUNTRY]]
  if(POLYMOD==FALSE) contact_home = inferred.hhcontact(HH = HH,AGEMAX = 16)
  if(POLYMOD==TRUE) contact_home = data.matrix(KAPPA_HOME[[COUNTRY]][1:16,1:16]*(HH[1:16,1:16]))
  return(contact_home)
}

HHcontact_Rest_of_world = function(COUNTRY){
  HH = HAM_WORLD[[(COUNTRY)]]
  contact_home = inferred.hhcontact(HH = HH,AGEMAX = 16)
  return(contact_home)
}

getWorkPop = function(COUNTRY){
  workingpop = as.numeric(work[work$iso3c %in% COUNTRY,6:21])
  workingpop[1:3] =0
  workingpop[15:16]=0.01
  wa = (workingpop)%*%t(workingpop)
  return(wa)
}

getSchoolPop = function(COUNTRY){
  schoolpop = as.numeric(school[school$iso %in% COUNTRY,grep(pattern = 'age',x = colnames(school))])
  sa = (schoolpop)#%*%t(schoolpop)+0.01
  sa = sa
  return(sa)
}
# getSchoolPop = function(COUNTRY){
#   schoolpop = as.numeric(school[school$Country.Code %in% COUNTRY,grep(pattern = 'age',x = colnames(school))])
#   sa = (schoolpop)%*%t(schoolpop) #+0.01
#   sa = sa
#   return(sa)
# }

# deprecated functions
# HHcontact_POLYMOD_DHS = function(COUNTRY,POLYMOD){
#   HH_temp = HHAGE[[paste0('hhage.',tolower(COUNTRY))]]
#   if(POLYMOD==FALSE) kappa.home = inferred.hhcontact(HH=HH_temp,z=16)
#   if(POLYMOD==TRUE) kappa.home = data.matrix(KAPPA.HOME[[paste0('lh.',tolower(COUNTRY))]][1:16,1:16]*(HH_temp[1:16,1:16]))
#   return(kappa.home)
# }
# 
# HHcontact_Rest_of_world = function(COUNTRY){
#   HH_temp = HAMworld[[(COUNTRY)]]
#   kappa.home = inferred.hhcontact(HH=HH_temp,z=16)
#   return(kappa.home)
# }
# 
# getWORKPYRAMID = function(COUNTRY){
#   temp = as.numeric(work[work$country==COUNTRY,8:20])
#   temp[3:13][is.na(temp[3:13])] = work$Total[work$final_country==COUNTRY]
#   temp[is.na(temp)] =0
#   pyramid = (temp)%*%t(temp)
#   pyramid = rbind(pyramid,0.0001,0.0001,0.0001)
#   pyramid = cbind(pyramid,0.0001,0.0001,0.0001)
#   return(pyramid)
# }
# 
# getSCHOOLPYRAMID = function(COUNTRY){
#   temp = as.numeric(school[school$final_country==COUNTRY,5:20])
#   pyramid = (temp)%*%t(temp) +0.01
#   return(pyramid)
# }



