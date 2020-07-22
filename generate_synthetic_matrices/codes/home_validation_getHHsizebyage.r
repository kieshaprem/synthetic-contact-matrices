#########################################################################################################################################
# FUNCTIONS
#########################################################################################################################################

getHHsize = function(hh)
{
  hh$agegphh = (hh$hv105 %/% 5)+1
  sizehh = data.frame(age = seq(1,max(hh$agegphh,na.rm = TRUE),1), 
                      mean = NA, median = NA, p2.5 = NA, p97.5 = NA, q1 =NA,q3 = NA)
  for(age in 1:nrow(sizehh))
  {
    index = which(hh$agegphh %in% age)
    sizehh$mean[age] =  mean(hh$hv009[index] - 1,na.rm = TRUE)
    sizehh$median[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.5,na.rm = TRUE))
    sizehh$p2.5[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.025,na.rm = TRUE))
    sizehh$p97.5[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.975,na.rm = TRUE))
    sizehh$q1[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.25,na.rm = TRUE))
    sizehh$q3[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.75,na.rm = TRUE))
   }
  return(sizehh)
}

getHHsizeRural = function(hh)
{
  hh = hh[tolower(hh$hv025) %in% 'rural',]
  hh$agegphh = (hh$hv105 %/% 5)+1
  sizehh = data.frame(age = seq(1,max(hh$agegphh,na.rm = TRUE),1), 
                      mean = NA, median = NA, p2.5 = NA, p97.5 = NA, q1 =NA,q3 = NA)
  for(age in 1:nrow(sizehh))
  {
    index = which(hh$agegphh %in% age)
    sizehh$mean[age] =  mean(hh$hv009[index] - 1,na.rm = TRUE)
    sizehh$median[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.5,na.rm = TRUE))
    sizehh$p2.5[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.025,na.rm = TRUE))
    sizehh$p97.5[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.975,na.rm = TRUE))
    sizehh$q1[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.25,na.rm = TRUE))
    sizehh$q3[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.75,na.rm = TRUE))
  }
  return(sizehh)
}

getHHsizeUrban = function(hh)
{
  hh = hh[tolower(hh$hv025) %in% 'urban',]
  hh$agegphh = (hh$hv105 %/% 5)+1
  sizehh = data.frame(age = seq(1,max(hh$agegphh,na.rm = TRUE),1), 
                      mean = NA, median = NA, p2.5 = NA, p97.5 = NA, q1 =NA,q3 = NA)
  for(age in 1:nrow(sizehh))
  {
    index = which(hh$agegphh %in% age)
    sizehh$mean[age] =  mean(hh$hv009[index] - 1,na.rm = TRUE)
    sizehh$median[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.5,na.rm = TRUE))
    sizehh$p2.5[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.025,na.rm = TRUE))
    sizehh$p97.5[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.975,na.rm = TRUE))
    sizehh$q1[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.25,na.rm = TRUE))
    sizehh$q3[age] =  as.numeric(quantile(hh$hv009[index] - 1,probs = 0.75,na.rm = TRUE))
  }
  return(sizehh)
}



#########################################################################################################################################
library(foreign)
library(tools)
library(countrycode)


hhsizebyage = hhsizebyage_urban = hhsizebyage_rural = list()


# afghanistan
rawdata = read.dta('input/household/afghanistan/AFPR70DT/AFPR70FL.DTA')
iso = "AFG"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# angola
rawdata = read.dta('input/household/angola/AOPR71DT/AOPR71FL.DTA')
iso = "AGO"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# BANGLADESH
rawdata = read.dta('input/household/bangladesh/BDPR72DT/BDPR72FL.DTA')
iso = "BGD"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# benin
rawdata = read.dta('input/household/benin/BJPR71DT/BJPR71FL.DTA')
iso = "BEN"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# BOLIVIA
rawdata = read.dta('input/household/bolivia/BOPR51DT/BOPR51FL.DTA')
iso = "BOL"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# cambodia
rawdata = read.dta('input/household/cambodia/KHPR73DT/KHPR73FL.DTA')
iso = "KHM"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# cameroon
rawdata = read.dta('input/household/cameroon/CMPR61DT/CMPR61FL.DTA')
iso = "CMR"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# chad
rawdata = read.dta('input/household/chad/TDPR71DT/TDPR71FL.DTA')
iso = "TCD"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# colombia
rawdata = read.dta('input/household/colombia/COPR72DT/COPR72FL.DTA')
iso = "COL"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# congo
rawdata = read.dta('input/household/congo/CGPR61DT/CGPR61FL.DTA')
iso = "COG"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# congodemocraticrepublic
rawdata = read.dta('input/household/congodemocraticrepublic/CDPR61DT/CDPR61FL.DTA')
iso = "COD"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# dominicanrepublic
rawdata = read.dta('input/household/dominicanrepublic/DRPR61DT/DRPR61FL.DTA')
iso = "DOM"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# ethiopia
rawdata = read.dta('input/household/ethiopia/ETPR71DT/ETPR71FL.DTA')
iso = "ETH"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# GHANA
rawdata = read.dta('input/household/ghana/GHPR72DT/GHPR72FL.DTA')
iso = "GHA"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# guatemala
rawdata = read.dta('input/household/guatemala/GUPR71DT/GUPR71FL.DTA')
iso = "GTM"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# guinea
rawdata = read.dta('input/household/guinea/GNPR71DT/GNPR71FL.DTA')
iso = "GIN"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# guyana
rawdata = read.dta('input/household/guyana/GYPR5IDT/GYPR5IFL.DTA')
iso = "GUY"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# haiti
rawdata = read.dta('input/household/haiti/HTPR71DT/HTPR71FL.DTA')
iso = "HTI"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# honduras
rawdata = read.dta('input/household/honduras/HNPR62DT/HNPR62FL.DTA')
iso = "HND"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# INDIA
rawdata = read.dta('input/household/india/IAPR74DT/IAPR74FL.DTA')
iso = "IND"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# INDONESIA
rawdata = read.dta('input/household/indonesia/IDPR71DT/IDPR71FL.DTA')
iso = "IDN"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# kenya 
rawdata = read.dta('input/household/kenya/KEPR72DT/KEPR72FL.DTA')
iso = "KEN"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# stopped here 

# kyrgyzrepublic 
rawdata = read.dta('input/household/kyrgyzrepublic/KYPR61DT/KYPR61FL.DTA')
iso = "KGZ"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# lesotho 
rawdata = read.dta('input/household/lesotho/LSPR71DT/LSPR71FL.DTA')
iso = "LSO"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# liberia
rawdata = read.dta('input/household/liberia/LBPR6ADT/LBPR6AFL.DTA')
iso = "LBR"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# malawi 
rawdata = read.dta('input/household/malawi/MWPR7ADT/MWPR7AFL.DTA')
iso = "MWI"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# maldives 
rawdata = read.dta('input/household/maldives/MVPR71DT/MVPR71FL.DTA')
iso = "MDV"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# mali 
rawdata = read.dta('input/household/mali/MLPR7HDT/MLPR7HFL.DTA')
iso = "MLI"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# nepal 
rawdata = read.dta('input/household/nepal/NPPR7HDT/NPPR7HFL.DTA')
iso = "NPL"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# niger 
rawdata = read.dta('input/household/niger/NIPR61DT/NIPR61FL.DTA')
iso = "NER"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# nigeria  
rawdata = read.dta('input/household/nigeria/NGPR7ADT/NGPR7AFL.DTA')
iso = "NGA"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# pakistan 
rawdata = read.dta('input/household/pakistan/PKPR71DT/PKPR71FL.DTA')
iso = "PAK"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# peru 
rawdata = read.dta('input/household/peru/PEPR6IDT/PEPR6IFL.DTA')
iso = "PER"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# PHILIPPINES 
rawdata = read.dta('input/household/philippines/PHPR71DT/PHPR71FL.DTA')
iso = "PHL"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# senegal 
rawdata = read.dta('input/household/senegal/SNPR7ZDT/SNPR7ZFL.DTA')
iso = "SEN"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# SIERRA LEONE  
rawdata = read.dta('input/household/sierraleone/SLPR61DT/SLPR61FL.DTA')
iso = "SLE"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# southafrica  
rawdata = read.dta('input/household/southafrica/ZAPR71DT/ZAPR71FL.DTA')
iso = "ZAF"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


# timorleste  
rawdata = read.dta('input/household/timorleste/TLPR71DT/TLPR71FL.DTA')
iso = "TLS"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# togo  
rawdata = read.dta('input/household/togo/TGPR61DT/TGPR61FL.DTA')
iso = "TGO"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


#UGANDA
rawdata = read.dta('input/household/uganda/UGPR7BDT/UGPR7BFL.DTA')
iso = "UGA"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


#vietnam
rawdata = read.dta('input/household/vietnam/VNPR53DT/VNPR53FL.DTA')
iso = "VNM"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)

# ZAMBIA
rawdata = read.dta('input/household/zambia/ZMPR71DT/ZMPR71FL.DTA')
iso = "ZMB"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


#zimbabwe
rawdata = read.dta('input/household/zimbabwe/ZWPR72DT/ZWPR72FL.DTA')
iso = "ZWE"
hhsizebyage[[iso]] = getHHsize(hh = rawdata)
hhsizebyage_urban[[iso]] = getHHsizeUrban(hh = rawdata)
hhsizebyage_rural[[iso]] = getHHsizeRural(hh = rawdata)
print(iso);rm(rawdata,iso)


save(hhsizebyage,file='output/hhsizebyage.rdata')
save(hhsizebyage_urban,file='output/hhsizebyage_urban.rdata')
save(hhsizebyage_rural,file='output/hhsizebyage_rural.rdata')
