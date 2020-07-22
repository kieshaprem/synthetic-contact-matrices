library(wbstats)
library(countrycode)
options(scipen=999)
str(wb_cachelist, max.level = 1)
# new_cache <- wbcache()
load('input/pop/popratio.rdata')
source('codes/contactmatricesworld_function.r')


indicators = c('SH.DYN.MORT','SH.DYN.AIDS.ZS','NY.GDP.MKTP.PP.CD','EN.POP.DNST','SP.RUR.TOTL.ZS','SP.URB.TOTL.IN.ZS',
               'SM.POP.NETM','AG.LND.AGRI.ZS','SH.TBS.INCD', 'EG.ELC.ACCS.ZS','SE.XPD.TOTL.GD.ZS','SP.POP.GROW',
               'SP.DYN.TFRT.IN','SH.XPD.CHEX.PP.CD','NE.EXP.GNFS.ZS','SL.UEM.TOTL.FE.ZS','IT.CEL.SETS.P2','SE.PRM.CMPT.ZS',
               'SL.UEM.TOTL.NE.ZS','TX.VAL.MRCH.CD.WT','SH.MMR.RISK.ZS','SH.STA.TRAF.P5','SP.ADO.TFRT','SP.DYN.AMRT.MA',
               'SE.SEC.CUAT.UP.ZS','SH.XPD.CHEX.PC.CD','SP.DYN.LE00.FE.IN','IT.NET.USER.ZS')

all_u5mr =  wb(indicator = "SH.DYN.MORT", startdate = 2000, enddate = 2020)
all_plhiv =  wb(indicator = "SH.DYN.AIDS.ZS", startdate = 2000, enddate = 2020)
all_gdppc =  wb(indicator = "NY.GDP.PCAP.CD", startdate = 2000, enddate = 2020)
all_popdense =  wb(indicator = "EN.POP.DNST", startdate = 2000, enddate = 2020)
all_ruralpop =  wb(indicator = "SP.RUR.TOTL.ZS", startdate = 2000, enddate = 2020)
all_urbanpop =  wb(indicator = "SP.URB.TOTL.IN.ZS", startdate = 2000, enddate = 2020)
all_netmigrate =  wb(indicator = "SM.POP.NETM", startdate = 2000, enddate = 2020)
all_agriculture =  wb(indicator = "AG.LND.AGRI.ZS", startdate = 2000, enddate = 2020)
all_tbinc =  wb(indicator = "SH.TBS.INCD", startdate = 2000, enddate = 2020)
all_electricity =  wb(indicator = "EG.ELC.ACCS.ZS", startdate = 2000, enddate = 2020)
all_eduexpense =  wb(indicator = "SE.XPD.TOTL.GD.ZS", startdate = 2000, enddate = 2020)
all_popgrowth =  wb(indicator = "SP.POP.GROW", startdate = 2000, enddate = 2020)
all_tfr  =  wb(indicator = "SP.DYN.TFRT.IN", startdate = 2000, enddate = 2020)
all_lifeexpectmale = wb(indicator = "SP.DYN.LE00.MA.IN", startdate = 2000, enddate = 2020)
all_exportsgoodsgdp = wb(indicator = "NE.EXP.GNFS.ZS", startdate = 2000, enddate = 2020)
all_unemployfemale = wb(indicator = "SL.UEM.TOTL.FE.ZS", startdate = 2000, enddate = 2020)
all_mobilephonesub = wb(indicator = "IT.CEL.SETS.P2", startdate = 2000, enddate = 2020)
all_primarycomplete = wb(indicator = "SE.PRM.CMPT.ZS", startdate = 2000, enddate = 2020)
all_unemployment = wb(indicator = "SL.UEM.TOTL.NE.ZS", startdate = 2000, enddate = 2020)
all_merchandiseexports = wb(indicator = "TX.VAL.MRCH.CD.WT", startdate = 2000, enddate = 2020)
all_riskmaternaldeath = wb(indicator = "SH.MMR.RISK.ZS", startdate = 2000, enddate = 2020)
all_mortalityrta = wb(indicator = "SH.STA.TRAF.P5", startdate = 2000, enddate = 2020)
all_adolescentfert = wb(indicator = "SP.ADO.TFRT", startdate = 2000, enddate = 2020)
all_malemortality = wb(indicator = "SP.DYN.AMRT.MA", startdate = 2000, enddate = 2020)
all_edusec = wb(indicator = "SE.SEC.CUAT.UP.ZS", startdate = 2000, enddate = 2020)
# all_healthexpenditure = wb(indicator = "SH.XPD.CHEX.PP.CD", startdate = 2000, enddate = 2020)
all_lifeexpectfemale = wb(indicator = "SP.DYN.LE00.FE.IN", startdate = 2000, enddate = 2020)
all_internet = wb(indicator = "IT.NET.USER.ZS", startdate = 2000, enddate = 2020)

cleaned_u5mr  = getLatestData_indicator(ALLDATA = all_u5mr)
cleaned_plhiv = getLatestData_indicator(ALLDATA = all_plhiv)
cleaned_gdppc = getLatestData_indicator(ALLDATA = all_gdppc)
cleaned_popdense = getLatestData_indicator(ALLDATA = all_popdense)
cleaned_ruralpop = getLatestData_indicator(ALLDATA = all_ruralpop)
cleaned_urbanpop = getLatestData_indicator(ALLDATA = all_urbanpop)
cleaned_netmigrate = getLatestData_indicator(ALLDATA = all_netmigrate)
cleaned_agriculture = getLatestData_indicator(ALLDATA = all_agriculture)
cleaned_tbinc = getLatestData_indicator(ALLDATA = all_tbinc)
cleaned_electricity = getLatestData_indicator(ALLDATA = all_electricity)
cleaned_eduexpense = getLatestData_indicator(ALLDATA = all_eduexpense)
cleaned_popgrowth = getLatestData_indicator(ALLDATA = all_popgrowth)
cleaned_tfr = getLatestData_indicator(ALLDATA = all_tfr)
cleaned_lifeexpectmale = getLatestData_indicator(ALLDATA = all_lifeexpectmale)
cleaned_exportsgoodsgdp = getLatestData_indicator(ALLDATA = all_exportsgoodsgdp)
cleaned_unemployfemale = getLatestData_indicator(ALLDATA = all_unemployfemale)
cleaned_mobilephonesub = getLatestData_indicator(ALLDATA = all_mobilephonesub)
cleaned_primarycomplete = getLatestData_indicator(ALLDATA = all_primarycomplete)
cleaned_unemployment = getLatestData_indicator(ALLDATA = all_unemployment)
cleaned_merchandiseexports = getLatestData_indicator(ALLDATA = all_merchandiseexports)
cleaned_riskmaternaldeath = getLatestData_indicator(ALLDATA = all_riskmaternaldeath)
cleaned_mortalityrta = getLatestData_indicator(ALLDATA = all_mortalityrta)
cleaned_adolescentfert = getLatestData_indicator(ALLDATA = all_adolescentfert)
cleaned_malemortality = getLatestData_indicator(ALLDATA = all_malemortality)
cleaned_edusec = getLatestData_indicator(ALLDATA = all_edusec)
# cleaned_healthexpenditure = getLatestData_indicator(ALLDATA = all_healthexpenditure)
cleaned_lifeexpectfemale = getLatestData_indicator(ALLDATA = all_lifeexpectfemale)
cleaned_internet = getLatestData_indicator(ALLDATA = all_internet)

indicator_list = list(popgrowth = cleaned_popgrowth,
                      popdense = cleaned_popdense,
                      tfr = cleaned_tfr,
                      lifeexpectmale = cleaned_lifeexpectmale,
                      urbanpop = cleaned_urbanpop,
                      ruralpop = cleaned_ruralpop,
                      unemployfemale = cleaned_unemployfemale,
                      netmigrate = cleaned_netmigrate,
                      gdppc = cleaned_gdppc,
                      eduexpense = cleaned_eduexpense,
                      exportsgoodsgdp = cleaned_exportsgoodsgdp,
                      primarycomplete = cleaned_primarycomplete,
                      u5mr = cleaned_u5mr,
                      plhiv = cleaned_plhiv,
                      tbinc = cleaned_tbinc,
                      agriculture = cleaned_agriculture,
                      electricity = cleaned_electricity,
                      mobilephonesub = cleaned_mobilephonesub,
                      unemployment = cleaned_unemployment,
                      merchandiseexports = cleaned_merchandiseexports,
                      riskmaternaldeath = cleaned_riskmaternaldeath,
                      mortalityrta = cleaned_mortalityrta,
                      adolescentfert = cleaned_adolescentfert,
                      malemortality = cleaned_malemortality,
                      edusec = cleaned_edusec,
                      # healthexpenditure = cleaned_healthexpenditure,
                      lifeexpectfemale = cleaned_lifeexpectfemale,
                      internet = cleaned_internet)


i=1
iso = as.character(indicator_list[[i]]$iso3c)

for(i in 2:length(indicator_list))
{
  extra = as.character(indicator_list[[i]]$iso3c)[!(as.character(indicator_list[[i]]$iso3c) %in% iso)]
  if(length(extra)>0) iso = c(iso,extra)
  iso = sort(iso)
}

temp = list()
for(j in 1:length(indicator_list))
{
  value = array(NA,length(iso))
  for(i in 1:length(iso))
  {
    index = which(indicator_list[[j]]$iso3c %in% iso[i])
    if(length(index)>0) value[i] = indicator_list[[j]]$value[index]
  }
  temp[[j]] = value
}

wbindicators = data.frame(iso = as.character(iso),do.call(cbind.data.frame, temp))
colnames(wbindicators)[2:ncol(wbindicators)] = names(indicator_list)


summary(wbindicators)
i=2
wbindicators[is.na(wbindicators[,i]),i] = -0.1
i=3
wbindicators[is.na(wbindicators[,i]),]
wbindicators[is.na(wbindicators[,i]),i] = c(25,20.3)
i=4
wbindicators[is.na(wbindicators[,i]),]
countrycode(sourcevar = wbindicators[is.na(wbindicators[,i]),1],origin = 'iso3c',destination = 'country.name')
wbindicators[is.na(wbindicators[,i]),i] = c(2.57,1.84,1.9,1.54,2.76,2.76,1.7,2.93,1.3)
i=5
wbindicators[is.na(wbindicators[,i]),]
countrycode(sourcevar = wbindicators[is.na(wbindicators[,i]),1],origin = 'iso3c',destination = 'country.name')
wbindicators[is.na(wbindicators[,i]),i] = c(81.2,71.06,76.8,89.4,74.5,63.6,76.4,64.01,77.5)
i=6 # do nothing
i=7 # do nothing
i=8
wbindicators[is.na(wbindicators[,i]),]
countrycode(sourcevar = wbindicators[is.na(wbindicators[,i]),1],origin = 'iso3c',destination = 'country.name')
wbindicators[is.na(wbindicators[,i]),i] 
i=9
wbindicators[is.na(wbindicators[,i]),]
countrycode(sourcevar = wbindicators[is.na(wbindicators[,i]),1],origin = 'iso3c',destination = 'country.name')
wbindicators[is.na(wbindicators[,i]),i] 
i=10
wbindicators[is.na(wbindicators[,i]),]
countrycode(sourcevar = wbindicators[is.na(wbindicators[,i]),1],origin = 'iso3c',destination = 'country.name')
wbindicators[is.na(wbindicators[,i]),i] 

index = 1:10
wbindicators[index,]
countrycode(sourcevar = wbindicators[index,1],origin = 'iso3c',destination = 'country.name')
wbindicators$unemployfemale[1] = 27.52
index = 11:20
wbindicators[index,]
countrycode(sourcevar = wbindicators[index,1],origin = 'iso3c',destination = 'country.name')
wbindicators$primarycomplete[12] = 100
wbindicators$primarycomplete[16] = 88.9
index = 21:30
wbindicators[index,]
countrycode(sourcevar = wbindicators[index,1],origin = 'iso3c',destination = 'country.name')
wbindicators$primarycomplete[23] = 66.7
wbindicators$u5mr[26] = 2
index = 31:40
wbindicators[index,]
index = 41:50
wbindicators[index,]
index = 51:60
wbindicators[index,]
index = 61:70
wbindicators[index,]
index = 71:80
wbindicators[index,]
index = 81:90
wbindicators[index,]
index = 91:100
wbindicators[index,]
wbindicators$u5mr[95] = 2.08
wbindicators$primarycomplete[99] = 48.5
index = 100:110
wbindicators[index,]
wbindicators$primarycomplete[109] = 95.86
index = 111:120
wbindicators[index,]
wbindicators$primarycomplete[117] = 101.78

index = 121:130
wbindicators[index,]
wbindicators$unemployfemale[122] = 6.63
wbindicators$unemployfemale[123] = 6.6
wbindicators$primarycomplete[128] = 71.88
wbindicators$primarycomplete[130] = 88.002

index = 131:140
wbindicators[index,]


wbindicators = wbindicators[(wbindicators$iso %in% popratio$iso3c),]

save(wbindicators,file = 'input/wbindicators.rdata')


# countrycode(sourcevar = wbindicators[index,1],origin = 'iso3c',destination = 'country.name')




