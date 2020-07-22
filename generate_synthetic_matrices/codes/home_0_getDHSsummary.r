library(foreign)
library(tools)
library(countrycode)
load('output/hamdhs.rdata')
getNoHouseholds = function(data)
{
  return(length(unique(data$hhid)))
}

dhsdata = data.frame(iso = names(HAM_DHS),individuals = NA, household = NA)


# afghanistan
rawdata = read.dta('input/household/afghanistan/AFPR70DT/AFPR70FL.DTA')
iso = "AFG"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# angola
rawdata = read.dta('input/household/angola/AOPR71DT/AOPR71FL.DTA')
iso = "AGO"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# BANGLADESH
rawdata = read.dta('input/household/bangladesh/BDPR72DT/BDPR72FL.DTA')
iso = "BGD"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# benin
rawdata = read.dta('input/household/benin/BJPR71DT/BJPR71FL.DTA')
iso = "BEN"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# BOLIVIA
rawdata = read.dta('input/household/bolivia/BOPR51DT/BOPR51FL.DTA')
iso = "BOL"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# cambodia
rawdata = read.dta('input/household/cambodia/KHPR73DT/KHPR73FL.DTA')
iso = "KHM"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# cameroon
rawdata = read.dta('input/household/cameroon/CMPR61DT/CMPR61FL.DTA')
iso = "CMR"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# chad
rawdata = read.dta('input/household/chad/TDPR71DT/TDPR71FL.DTA')
iso = "TCD"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# colombia
rawdata = read.dta('input/household/colombia/COPR72DT/COPR72FL.DTA')
iso = "COL"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# congo
rawdata = read.dta('input/household/congo/CGPR61DT/CGPR61FL.DTA')
iso = "COG"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# congodemocraticrepublic
rawdata = read.dta('input/household/congodemocraticrepublic/CDPR61DT/CDPR61FL.DTA')
iso = "COD"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# dominicanrepublic
rawdata = read.dta('input/household/dominicanrepublic/DRPR61DT/DRPR61FL.DTA')
iso = "DOM"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# ethiopia
rawdata = read.dta('input/household/ethiopia/ETPR71DT/ETPR71FL.DTA')
iso = "ETH"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# GHANA
rawdata = read.dta('input/household/ghana/GHPR72DT/GHPR72FL.DTA')
iso = "GHA"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# guatemala
rawdata = read.dta('input/household/guatemala/GUPR71DT/GUPR71FL.DTA')
iso = "GTM"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# guinea
rawdata = read.dta('input/household/guinea/GNPR71DT/GNPR71FL.DTA')
iso = "GIN"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# guyana
rawdata = read.dta('input/household/guyana/GYPR5IDT/GYPR5IFL.DTA')
iso = "GUY"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# haiti
rawdata = read.dta('input/household/haiti/HTPR71DT/HTPR71FL.DTA')
iso = "HTI"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# honduras
rawdata = read.dta('input/household/honduras/HNPR62DT/HNPR62FL.DTA')
iso = "HND"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# INDIA
rawdata = read.dta('input/household/india/IAPR74DT/IAPR74FL.DTA')
iso = "IND"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# INDONESIA
rawdata = read.dta('input/household/indonesia/IDPR71DT/IDPR71FL.DTA')
iso = "IDN"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# kenya 
rawdata = read.dta('input/household/kenya/KEPR72DT/KEPR72FL.DTA')
iso = "KEN"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# stopped here 

# kyrgyzrepublic 
rawdata = read.dta('input/household/kyrgyzrepublic/KYPR61DT/KYPR61FL.DTA')
iso = "KGZ"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# lesotho 
rawdata = read.dta('input/household/lesotho/LSPR71DT/LSPR71FL.DTA')
iso = "LSO"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# liberia
rawdata = read.dta('input/household/liberia/LBPR6ADT/LBPR6AFL.DTA')
iso = "LBR"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# malawi 
rawdata = read.dta('input/household/malawi/MWPR7ADT/MWPR7AFL.DTA')
iso = "MWI"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# maldives 
rawdata = read.dta('input/household/maldives/MVPR71DT/MVPR71FL.DTA')
iso = "MDV"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# mali 
rawdata = read.dta('input/household/mali/MLPR7HDT/MLPR7HFL.DTA')
iso = "MLI"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# nepal 
rawdata = read.dta('input/household/nepal/NPPR7HDT/NPPR7HFL.DTA')
iso = "NPL"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# niger 
rawdata = read.dta('input/household/niger/NIPR61DT/NIPR61FL.DTA')
iso = "NER"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# nigeria  
rawdata = read.dta('input/household/nigeria/NGPR7ADT/NGPR7AFL.DTA')
iso = "NGA"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# pakistan 
rawdata = read.dta('input/household/pakistan/PKPR71DT/PKPR71FL.DTA')
iso = "PAK"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# peru 
rawdata = read.dta('input/household/peru/PEPR6IDT/PEPR6IFL.DTA')
iso = "PER"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# PHILIPPINES 
rawdata = read.dta('input/household/philippines/PHPR71DT/PHPR71FL.DTA')
iso = "PHL"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# senegal 
rawdata = read.dta('input/household/senegal/SNPR7ZDT/SNPR7ZFL.DTA')
iso = "SEN"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# SIERRA LEONE  
rawdata = read.dta('input/household/sierraleone/SLPR61DT/SLPR61FL.DTA')
iso = "SLE"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# southafrica  
rawdata = read.dta('input/household/southafrica/ZAPR71DT/ZAPR71FL.DTA')
iso = "ZAF"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


# timorleste  
rawdata = read.dta('input/household/timorleste/TLPR71DT/TLPR71FL.DTA')
iso = "TLS"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# togo  
rawdata = read.dta('input/household/togo/TGPR61DT/TGPR61FL.DTA')
iso = "TGO"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


#UGANDA
rawdata = read.dta('input/household/uganda/UGPR7BDT/UGPR7BFL.DTA')
iso = "UGA"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


#vietnam
rawdata = read.dta('input/household/vietnam/VNPR53DT/VNPR53FL.DTA')
iso = "VNM"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)

# ZAMBIA
rawdata = read.dta('input/household/zambia/ZMPR71DT/ZMPR71FL.DTA')
iso = "ZMB"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


#zimbabwe
rawdata = read.dta('input/household/zimbabwe/ZWPR72DT/ZWPR72FL.DTA')
iso = "ZWE"
dhsdata$individuals[dhsdata$iso %in% iso] = nrow(rawdata)
dhsdata$household[dhsdata$iso %in% iso] = getNoHouseholds(rawdata)
rm(rawdata);print(iso)


save(dhsdata,file='input/household/dhsdata.rdata')


# load('input/household/dhsdata.rdata')
# load('input/pop/popratio.rdata')
# dhsdata$country = popratio$countryname[match(dhsdata$iso,popratio$iso3c)]
# write.csv(dhsdata,file = 'input/household/dhsdata.csv',row.names = FALSE)
