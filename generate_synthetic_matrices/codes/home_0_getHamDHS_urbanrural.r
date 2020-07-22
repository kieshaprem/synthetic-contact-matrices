#########################################################################################################################################
# FUNCTIONS
#########################################################################################################################################

cleaner = function(hh)
{
  hh_urban = hh[tolower(hh$hv025) %in% 'urban',]
  unihh=unique(hh_urban$hhid)
  age1=age2=id1=id2=vector("list", length = length(unihh))
  for(h in 1:length(unihh))
  {
    if(h %% 1000 == 0) print(h)
    J=which(hh_urban$hhid==unihh[h])
    maxhh = hh_urban$hv009[J[1]]
    if(maxhh > 1)
    {
      i1 = rep(1:maxhh,maxhh)
      i2 = rep(1:maxhh,each=maxhh)
      ages = hh_urban$hv105[J]
      a1 = ages[i1]
      a2 = ages[i2]
      age1[[h]]=a1
      age2[[h]]=a2
      id1[[h]]=i1
      id2[[h]]=i2
    }
  }
  urban = list(age1=unlist(age1),
               age2=unlist(age2),
               id1=unlist(id1),
               id2=unlist(id2))
  
  hh_rural = hh[tolower(hh$hv025) %in% 'rural',]
  unihh=unique(hh_rural$hhid)
  age1=age2=id1=id2=vector("list", length = length(unihh))
  for(h in 1:length(unihh))
  {
    if(h %% 1000 == 0) print(h)
    J=which(hh_rural$hhid==unihh[h])
    maxhh = hh_rural$hv009[J[1]]
    if(maxhh > 1)
    {
      i1 = rep(1:maxhh,maxhh)
      i2 = rep(1:maxhh,each=maxhh)
      ages = hh_rural$hv105[J]
      a1 = ages[i1]
      a2 = ages[i2]
      age1[[h]]=a1
      age2[[h]]=a2
      id1[[h]]=i1
      id2[[h]]=i2
    }
  }
  rural = list(age1=unlist(age1),
               age2=unlist(age2),
               id1=unlist(id1),
               id2=unlist(id2))
  hhout = list(urban = urban, rural = rural)
  return(hhout)
}

getHHage = function(hh)
{ 
  data = hh$urban
  id1 = data$id1
  id2 = data$id2
  age1 = data$age1
  age2 = data$age2
  i=which(id1==id2)
  age1=age1[-i]
  age2=age2[-i]
  id1=id1[-i]
  id2=id2[-i]
  
  M=matrix(0,100,100)
  M5=matrix(0,20,20)
  
  for(i in 1:100)
  {
    cat(i)
    for(j in 1:100)M[i,j]=sum((age1 == i)*(age2 == j),na.rm = TRUE)
  }
  
  for(i in 1:100)
  {
    i5=ceiling(i/5)
    for(j in 1:100)
    {
      j5=ceiling(j/5)
      M5[i5,j5]=M5[i5,j5]+M[i,j]
    }
  }
  urban = M5 
  
  data = hh$rural
  id1 = data$id1
  id2 = data$id2
  age1 = data$age1
  age2 = data$age2
  i=which(id1==id2)
  age1=age1[-i]
  age2=age2[-i]
  id1=id1[-i]
  id2=id2[-i]
  
  M=matrix(0,100,100)
  M5=matrix(0,20,20)
  
  for(i in 1:100)
  {
    cat(i)
    for(j in 1:100)M[i,j]=sum((age1 == i)*(age2 == j),na.rm = TRUE)
  }
  
  for(i in 1:100)
  {
    i5=ceiling(i/5)
    for(j in 1:100)
    {
      j5=ceiling(j/5)
      M5[i5,j5]=M5[i5,j5]+M[i,j]
    }
  }
  rural = M5
  
  M5 = list(urban = urban, rural = rural)

  return(M5)
}

getAgeProfile = function(hh)
{
  tabulate((1+floor(hh$hv105/5)))
}


getHAMdhs = function(ISO,AGEMAX=16)
{
  hhage_urban = data.matrix(HHAGE_DHS_URBAN[[ISO]])
  hhage_rural = data.matrix(HHAGE_DHS_RURAL[[ISO]])
  
  ageindiv_urban = AGE_DHS_URBAN[[ISO]]
  ageindiv_rural = AGE_DHS_RURAL[[ISO]]
  
  HH5_URBAN = HH5_RURAL = matrix(0,20,20)
  for(indiv in 1:20)
  {
    HH5_URBAN[indiv,] = hhage_urban[indiv,]/ageindiv_urban[indiv]
    HH5_RURAL[indiv,] = hhage_rural[indiv,]/ageindiv_rural[indiv]
  }
  HH5_URBAN = HH5_URBAN[1:AGEMAX,1:AGEMAX]
  HH5_RURAL = HH5_RURAL[1:AGEMAX,1:AGEMAX]
  HH5 = list(urban = HH5_URBAN,rural = HH5_RURAL)
  # image(HH5)
  return(HH5)
}




#########################################################################################################################################
library(foreign)
library(tools)
library(countrycode)

DHScountries = list.files(path = 'input/household/')
DHScountries_proper = DHScountries
DHScountries_proper[DHScountries %in% "congodemocraticrepublic"] = 'DR Congo'
DHScountries_proper[DHScountries %in% "dominicanrepublic"] = 'dominican republic'
DHScountries_proper[DHScountries %in% "dominicanrepublic"] = 'dominican republic'
DHScountries_proper[DHScountries %in% "southafrica"] = 'south africa'
DHScountries_iso3c = countrycode(sourcevar =DHScountries_proper,origin = 'country.name', destination = 'iso3c')

AGE_DHS_URBAN = AGE_DHS_RURAL = HHAGE_DHS_URBAN = HAM_DHS_URBAN = HHAGE_DHS_RURAL = HAM_DHS_RURAL = list()
startTime = Sys.time()
# afghanistan
rawdata = read.dta('input/household/afghanistan/AFPR70DT/AFPR70FL.DTA')
iso = "AFG"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# angola
rawdata = read.dta('input/household/angola/AOPR71DT/AOPR71FL.DTA')
iso = "AGO"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# BANGLADESH
rawdata = read.dta('input/household/bangladesh/BDPR72DT/BDPR72FL.DTA')
iso = "BGD"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# benin
rawdata = read.dta('input/household/benin/BJPR71DT/BJPR71FL.DTA')
iso = "BEN"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# BOLIVIA
rawdata = read.dta('input/household/bolivia/BOPR51DT/BOPR51FL.DTA')
iso = "BOL"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# cambodia
rawdata = read.dta('input/household/cambodia/KHPR73DT/KHPR73FL.DTA')
iso = "KHM"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# cameroon
rawdata = read.dta('input/household/cameroon/CMPR61DT/CMPR61FL.DTA')
iso = "CMR"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# chad
rawdata = read.dta('input/household/chad/TDPR71DT/TDPR71FL.DTA')
iso = "TCD"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# colombia
rawdata = read.dta('input/household/colombia/COPR72DT/COPR72FL.DTA')
iso = "COL"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# congo
rawdata = read.dta('input/household/congo/CGPR61DT/CGPR61FL.DTA')
iso = "COG"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# congodemocraticrepublic
rawdata = read.dta('input/household/congodemocraticrepublic/CDPR61DT/CDPR61FL.DTA')
iso = "COD"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# dominicanrepublic
rawdata = read.dta('input/household/dominicanrepublic/DRPR61DT/DRPR61FL.DTA')
iso = "DOM"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# ethiopia
rawdata = read.dta('input/household/ethiopia/ETPR71DT/ETPR71FL.DTA')
iso = "ETH"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# GHANA
rawdata = read.dta('input/household/ghana/GHPR72DT/GHPR72FL.DTA')
iso = "GHA"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# guatemala
rawdata = read.dta('input/household/guatemala/GUPR71DT/GUPR71FL.DTA')
iso = "GTM"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# guinea
rawdata = read.dta('input/household/guinea/GNPR71DT/GNPR71FL.DTA')
iso = "GIN"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# guyana
rawdata = read.dta('input/household/guyana/GYPR5IDT/GYPR5IFL.DTA')
iso = "GUY"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# haiti
rawdata = read.dta('input/household/haiti/HTPR71DT/HTPR71FL.DTA')
iso = "HTI"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# honduras
rawdata = read.dta('input/household/honduras/HNPR62DT/HNPR62FL.DTA')
iso = "HND"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# INDIA
rawdata = read.dta('input/household/india/IAPR74DT/IAPR74FL.DTA')
iso = "IND"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# INDONESIA
rawdata = read.dta('input/household/indonesia/IDPR71DT/IDPR71FL.DTA')
iso = "IDN"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# kenya 
rawdata = read.dta('input/household/kenya/KEPR72DT/KEPR72FL.DTA')
iso = "KEN"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)
 

# kyrgyzrepublic 
rawdata = read.dta('input/household/kyrgyzrepublic/KYPR61DT/KYPR61FL.DTA')
iso = "KGZ"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# lesotho 
rawdata = read.dta('input/household/lesotho/LSPR71DT/LSPR71FL.DTA')
iso = "LSO"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# liberia
rawdata = read.dta('input/household/liberia/LBPR6ADT/LBPR6AFL.DTA')
iso = "LBR"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# malawi 
rawdata = read.dta('input/household/malawi/MWPR7ADT/MWPR7AFL.DTA')
iso = "MWI"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# maldives 
rawdata = read.dta('input/household/maldives/MVPR71DT/MVPR71FL.DTA')
iso = "MDV"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# mali 
rawdata = read.dta('input/household/mali/MLPR7HDT/MLPR7HFL.DTA')
iso = "MLI"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# nepal 
rawdata = read.dta('input/household/nepal/NPPR7HDT/NPPR7HFL.DTA')
iso = "NPL"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# niger 
rawdata = read.dta('input/household/niger/NIPR61DT/NIPR61FL.DTA')
iso = "NER"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# nigeria  
rawdata = read.dta('input/household/nigeria/NGPR7ADT/NGPR7AFL.DTA')
iso = "NGA"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# pakistan 
rawdata = read.dta('input/household/pakistan/PKPR71DT/PKPR71FL.DTA')
iso = "PAK"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# peru 
rawdata = read.dta('input/household/peru/PEPR6IDT/PEPR6IFL.DTA')
iso = "PER"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# PHILIPPINES 
rawdata = read.dta('input/household/philippines/PHPR71DT/PHPR71FL.DTA')
iso = "PHL"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# senegal 
rawdata = read.dta('input/household/senegal/SNPR7ZDT/SNPR7ZFL.DTA')
iso = "SEN"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# SIERRA LEONE  
rawdata = read.dta('input/household/sierraleone/SLPR61DT/SLPR61FL.DTA')
iso = "SLE"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# southafrica  
rawdata = read.dta('input/household/southafrica/ZAPR71DT/ZAPR71FL.DTA')
iso = "ZAF"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


# timorleste  
rawdata = read.dta('input/household/timorleste/TLPR71DT/TLPR71FL.DTA')
iso = "TLS"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# togo  
rawdata = read.dta('input/household/togo/TGPR61DT/TGPR61FL.DTA')
iso = "TGO"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


#UGANDA
rawdata = read.dta('input/household/uganda/UGPR7BDT/UGPR7BFL.DTA')
iso = "UGA"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


#vietnam
rawdata = read.dta('input/household/vietnam/VNPR53DT/VNPR53FL.DTA')
iso = "VNM"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

# ZAMBIA
rawdata = read.dta('input/household/zambia/ZMPR71DT/ZMPR71FL.DTA')
iso = "ZMB"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)


#zimbabwe
rawdata = read.dta('input/household/zimbabwe/ZWPR72DT/ZWPR72FL.DTA')
iso = "ZWE"
data = cleaner(rawdata)
AGE_DHS_URBAN[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'urban',])
AGE_DHS_RURAL[[iso]] = getAgeProfile(rawdata[tolower(rawdata$hv025) %in% 'rural',])
HHAGE_DHS = getHHage(data)
HHAGE_DHS_URBAN[[iso]] = HHAGE_DHS$urban
HHAGE_DHS_RURAL[[iso]] = HHAGE_DHS$rural
HAM = getHAMdhs(ISO = iso)
HAM_DHS_URBAN[[iso]] = HAM$urban
HAM_DHS_RURAL[[iso]] = HAM$rural
image(HHAGE_DHS_RURAL[[iso]])
image(HHAGE_DHS_URBAN[[iso]])
image(HAM_DHS_URBAN[[iso]])
image(HAM_DHS_RURAL[[iso]])
rm(rawdata,data,iso,HHAGE_DHS,HAM)

save(AGE_DHS_URBAN,file='output/agedhs_urban.rdata')
save(AGE_DHS_RURAL,file='output/agedhs_rural.rdata')
save(HHAGE_DHS_URBAN,file='output/hhagedhs_urban.rdata')
save(HHAGE_DHS_RURAL,file='output/hhagedhs_rural.rdata')
save(HAM_DHS_URBAN,file='output/hamdhs_urban.rdata')
save(HAM_DHS_RURAL,file='output/hamdhs_rural.rdata')
endTime = Sys.time()
endTime - startTime
