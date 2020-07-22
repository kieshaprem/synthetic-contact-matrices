library(countrycode)
load('input/school/pupiltoteacherratio.rdata')
load('input/work/workpopage.rdata')
load('input/work/workpopage_urban.rdata')
load('input/work/workpopage_rural.rdata')

# Responsive School Systems: Connecting Facilities, Sectors and Programmes for Student Success - © OECD 2018
# Chapter 3
# Version 1 - Last updated: 25-Sep-2018
# Disclaimer: http://oe.cd/disclaimer
# Figure 3.5 Differences in rural and urban schools’ student-teacher ratio and class size, 2015
# Source: OECD (2016), PISA 2015 Results (Volume II): Policies and Practices for Successful Schools, PISA, Tables II.6.29 and Table II.6.30.
diff_ptr = read.csv('input/school/oecd_diff_pupiltoteacherratio_urbanrural.csv',stringsAsFactors = FALSE)
diff_ptr$iso3c = countrycode::countrycode(sourcevar = diff_ptr$country,origin = 'country.name',destination = 'iso3c')
diff_ptr$incomegroup = workpopage$total$incomegroup[match(x = diff_ptr$iso3c, workpopage$total$iso3c)]

PTR_urban = PTR_rural = PTR

for(i in 1:nrow(PTR))
{
  ISO = (PTR$iso[i])
  if(!(ISO %in% diff_ptr$iso3c))
  {
    PTR_urban[PTR_urban$iso %in% ISO,2:5] = (2*PTR[PTR$iso %in% ISO,2:5]+diff_ptr$diff.urban.rural.[diff_ptr$country %in% 'OECD average'])/2
    PTR_rural[PTR_rural$iso %in% ISO,2:5] = PTR_urban[PTR_urban$iso %in% ISO,2:5] - diff_ptr$diff.urban.rural.[diff_ptr$country %in% 'OECD average']
  }
  if((ISO %in% diff_ptr$iso3c))
  {
    PTR_urban[PTR_urban$iso %in% ISO,2:5] = (2*PTR[PTR$iso %in% ISO,2:5]+diff_ptr$diff.urban.rural.[diff_ptr$iso3c %in% ISO])/2
    PTR_rural[PTR_rural$iso %in% ISO,2:5] = PTR_urban[PTR_urban$iso %in% ISO,2:5] - diff_ptr$diff.urban.rural.[diff_ptr$iso3c %in% ISO]
  }
}

save(PTR_urban,file = 'input/school/pupiltoteacherratio_urban.rdata')
save(PTR_rural,file = 'input/school/pupiltoteacherratio_rural.rdata')


