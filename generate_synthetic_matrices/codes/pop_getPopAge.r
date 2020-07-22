## POPULATION AGE STRUCTURE 
# United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.


library(countrycode)
# library(wpp2019)
options(scipen=999)
poptotal2020 = read.csv('input/pop/popage_total2020.csv',as.is = TRUE)
poptotal = poptotal2020
poptotal[,3:23] = poptotal2020[,3:23]*1000
poptotal = data.frame(iso3c =  countrycode(poptotal2020$Region..subregion..country.or.area..,origin = 'country.name', destination = 'iso3c'),
                      countryname = poptotal2020$Region..subregion..country.or.area..,
                      year = poptotal2020$Reference.date..as.of.1.July.,
                      poptotal2020[,3:23]*1000)
poptotal$total = rowSums(poptotal[,4:24])
poptotal = poptotal[order(poptotal$countryname),]

popratio = poptotal
popratio[,4:24] = (poptotal[,4:24]/poptotal$total)

save(poptotal,file = 'input/pop/poptotal.rdata')
save(popratio,file = 'input/pop/popratio.rdata')


popfemale2020 = read.csv('input/pop/popage_female2020.csv',as.is = TRUE)
popfemale = popfemale2020
popfemale[,3:23] = popfemale2020[,3:23]*1000
popfemale = data.frame(iso3c =  countrycode(popfemale2020$Region..subregion..country.or.area..,origin = 'country.name', destination = 'iso3c'),
                      countryname = popfemale2020$Region..subregion..country.or.area..,
                      year = popfemale2020$Reference.date..as.of.1.July.,
                      popfemale2020[,3:23]*1000)
popfemale$total = rowSums(popfemale[,4:24])
popfemale = popfemale[order(popfemale$countryname),]
save(popfemale,file = 'input/pop/popfemale.rdata')

popratio_female = popfemale
if(identical(poptotal$iso3c, popratio_female$iso3c))
{
  popratio_female[,4:24]=popratio_female[,4:24]/poptotal$total
  save(popratio_female,file = 'input/pop/popratio_female.rdata')
}



popmale2020 = read.csv('input/pop/popage_male2020.csv',as.is = TRUE)
popmale = popmale2020
popmale[,3:23] = popmale2020[,3:23]*1000
popmale = data.frame(iso3c =  countrycode(popmale2020$Region..subregion..country.or.area..,origin = 'country.name', destination = 'iso3c'),
                     countryname = popmale2020$Region..subregion..country.or.area..,
                     year = popmale2020$Reference.date..as.of.1.July.,
                     popmale2020[,3:23]*1000)
popmale$total = rowSums(popmale[,4:24])
popmale = popmale[order(popmale$countryname),]
save(popmale,file = 'input/pop/popmale.rdata')

popratio_male = popmale
if(identical(poptotal$iso3c, popratio_male$iso3c))
{
  popratio_male[,4:24]=popratio_male[,4:24]/poptotal$total
  save(popratio_male,file = 'input/pop/popratio_male.rdata')
}



