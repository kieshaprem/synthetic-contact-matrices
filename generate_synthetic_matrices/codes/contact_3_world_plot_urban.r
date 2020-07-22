library(countrycode)
library(rjags)
source('codes/functions_plotContactmatrices.r')

load('input/pop/popUrban.rdata')

load('output/syntheticcontactmatrices2020/urban/contact_home_urban.rdata')
load('output/syntheticcontactmatrices2020/urban/contact_work_urban.rdata')
load('output/syntheticcontactmatrices2020/urban/contact_school_urban.rdata')
load('output/syntheticcontactmatrices2020/urban/contact_others_urban.rdata')
load('output/syntheticcontactmatrices2020/urban/contact_all_urban.rdata')


countries = as.character(popUrban$ratio_female$iso3c[popUrban$ratio_female$iso3c %in% names(contact_all)]) # to reorder the countries by name (not ISO)


pdf(paste0('plots/contactmatrices2020_urban.pdf'),height=21*cm,width=13.8*cm)
for(i in 1:length(countries)){

  COUNTRY = countries[i]
  country_name = (popUrban$ratio_female$countryname[popUrban$ratio_female$iso3c %in% COUNTRY])

  max_home = ceiling(max(contact_home[[countries[i]]]))
  max_work = ceiling(max(contact_work[[countries[i]]]))
  max_school = ceiling(max(contact_school[[countries[i]]]))
  max_others = ceiling(max(contact_others[[countries[i]]]))
  max_all = ceiling(max(contact_all[[countries[i]]]))
  
  grid.newpage()
  
  pushViewport(plotViewport(c(1,1,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=4,ncol=2,width=c(1,1),height=c(0.1,1,1,1))))
  
  pushViewport(viewport(layout.pos.row=1))
  grid.text(country_name,x=0.5,y=.5,gp=gpar(fontsize=11,fontface='bold'))
  popViewport()
  
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 1))
  plotPopPyramidUrban(COUNTRY = COUNTRY,INDEX = 'a')
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
