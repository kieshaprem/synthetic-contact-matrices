library(countrycode)
library(rjags)
source('codes/functions_plotContactmatrices.r')

load('input/pop/popratio_female.rdata')
load('input/pop/popratio_male.rdata')

load('output/syntheticmatrices/contact_home.rdata')
load('output/syntheticmatrices/contact_work.rdata')
load('output/syntheticmatrices/contact_school.rdata')
load('output/syntheticmatrices/contact_others.rdata')
load('output/syntheticmatrices/contact_all.rdata')

countries = as.character(popratio_female$iso3c[popratio_female$iso3c %in% names(contact_all)]) # to reorder the countries by name (not ISO)


pdf(paste0('plots/contactmatrices2021.pdf'),height=21*cm,width=13.8*cm)
for(i in 1:length(countries)){
  
  COUNTRY = countries[i]
  country_name = (popratio_female$countryname[popratio_female$iso3c %in% COUNTRY])
  
  max_home = ceiling(max(contact_home[[countries[i]]]))+1
  max_work = ceiling(max(contact_work[[countries[i]]]))+1
  max_school = ceiling(max(contact_school[[countries[i]]]))+1
  max_others = ceiling(max(contact_others[[countries[i]]]))+1
  max_all = ceiling(max(contact_all[[countries[i]]]))+1
  
  grid.newpage()
  
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=4,ncol=2,width=c(1,1),height=c(0.1,1,1,1))))
  
  pushViewport(viewport(layout.pos.row=1))
  grid.text(country_name,x=0.5,y=.5,gp=gpar(fontsize=11,fontface='bold'))
  popViewport()
  
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 1))
  plotPopPyramid(COUNTRY = COUNTRY,INDEX = 'a')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 2))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(contact_home[[COUNTRY]],
                                                        max.value = max_home),LOCATION = 'Home',a=5,INDEX = 'b')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=3,layout.pos.col = 1))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(contact_work[[COUNTRY]],
                                                        max.value = max_work),LOCATION = 'Work',a=5,INDEX = 'c')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=3,layout.pos.col = 2))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(contact_school[[COUNTRY]],
                                                        max.value = max_school),LOCATION = 'School',a=5,INDEX = 'd')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=4,layout.pos.col = 1))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(contact_others[[COUNTRY]],
                                                        max.value = max_others),LOCATION = 'Other Locations',a=5,INDEX ='e')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=4,layout.pos.col = 2))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(contact_all[[COUNTRY]],
                                                        max.value = max_all),LOCATION = 'All locations',a=5,INDEX ='f')
  popViewport()
  
  popViewport()
  popViewport()
  
  if(i%%10==0)print(i)
}
dev.off()
