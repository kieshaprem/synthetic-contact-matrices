
library(grid)
library(grDevices)
library(gtools)
library(countrycode)
cm=1/2.54

##### DATA

# load in the data 
load('output/hamdhs.rdata')
load('output/hamdhs_urban.rdata')
load('output/hamdhs_rural.rdata')
load('output/hhsizebyage.rdata')
load('output/hhsizebyage_urban.rdata')
load('output/hhsizebyage_rural.rdata')
load('input/pop/popRural.rdata')
load('input/pop/popUrban.rdata')
load('input/pop/popratio_female.rdata')
load('input/pop/popratio_male.rdata')


# Country codes 
countries = names(HAM_DHS)


source('codes/contactmatricesworld_function.r')
source('codes/contactmatricesplot_function.r')

pdf(paste0('plots/HAM_DHS_urban_rural.pdf'),height=25*cm,width=24*cm)
for(co in 1:length(countries)) {
  # png(paste0('validation/validated_bootstrap_',countrieslowercase[c],'.png'),height=13,width=18,units = 'cm' ,res=700,pointsize=10)
  urban =HAM_DHS_URBAN[[countries[co]]][1:16,1:16]
  rural = HAM_DHS_RURAL[[countries[co]]][1:16,1:16]
  both = HAM_DHS[[countries[co]]][1:16,1:16]
  max_home = max(urban,rural,both)+0.05
  
  # png(paste0('validation/validated_bootstrap_',countrieslowercase[c],'.png'),height=9,width=23,units='cm',res=700,pointsize=10)
  grid.newpage()
  pushViewport(plotViewport(c(1,1,1,1)))
  
  pushViewport(viewport(layout=grid.layout(nrow=4,ncol=3,width=c(1,1,1),height=c(0.1,1,0.9,0.9))))
  pushViewport(viewport(layout.pos.row=1))
  countryname = popratio_female$countryname[popratio_female$iso3c %in% countries[co]]
  grid.text(paste('Empirical DHS household data :',countryname),
            x=unit(0,'npc'),just = 'left',
            gp=gpar(fontsize=13,fontface='bold'),y=.7)
  
  popViewport()
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 1))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(urban, max.value = max_home),
                       LOCATION = 'Urban Households',a=5,INDEX = 'a',CONTACT = FALSE)
  popViewport()
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 2))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(rural, max.value = max_home),
                       LOCATION = 'Rural Households',a=5,INDEX = 'b',CONTACT = FALSE)
  popViewport()
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 3))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(both, max.value = max_home),
                       LOCATION = 'Urban & Rural',a=5,INDEX = 'c',CONTACT = FALSE)
  popViewport()
  
  
  pushViewport(viewport(layout.pos.row=1,layout.pos.col = 3))
  if(1)
  {
    numberlegend = seq(0,max_home,0.01)
    colourlegend = colour_blue(numberlegend,max_home) 
    xposlegend = (seq(0.15,0.85,length.out = length(numberlegend)+1))
    yposlegend = c(0.65,1.0)
    for(i in 1:length(numberlegend)) grid.polygon(x=unit(c(xposlegend[i],xposlegend[i+1],xposlegend[i+1],xposlegend[i]),'npc'),
                                                  y=unit(c(yposlegend[1],yposlegend[1],yposlegend[2],yposlegend[2]),'npc')+unit(0.5,'lines'),
                                                  gp = gpar(fill=colourlegend[i],col=colourlegend[i]))
    grid.polygon(y=unit(c(yposlegend[1],yposlegend[1],yposlegend[2],yposlegend[2]),'npc')+unit(0.5,'lines'),
                 x=unit(c(max(xposlegend),min(xposlegend),min(xposlegend),max(xposlegend)),'npc'),
                 gp = gpar(fill=NA,col='black'))
    grid.text('Mean number of household members\nin a specific age group',x=unit(0.5,'npc'),y=unit(0.5,'lines'),default.units = 'npc',rot = 0,gp=gpar(fontsize=7,fontface='bold'))
    if(max_home <1.5) grid.text(seq(0,(max_home %/%0.25)*0.25,0.25),x=unit(xposlegend[match(seq(0,(max_home %/%0.25)*0.25,0.25),numberlegend)],'npc'),y=unit(1.75,'lines'), gp=gpar(fontsize=7,fontface='plain'))
    if(max_home >=1.5) grid.text(seq(0,(max_home %/%0.5)*0.5,0.5),x=unit(xposlegend[match(seq(0,(max_home %/%0.5)*0.5,0.5),numberlegend)],'npc'),y=unit(1.75,'lines'), gp=gpar(fontsize=7,fontface='plain'))
    
  }
  popViewport()
  
  pushViewport(viewport(layout.pos.row=3,layout.pos.col = 1))
  plotBarPlotbyAge(DATA = hhsizebyage_urban[[countries[co]]],LEGEND = TRUE,INDEX = 'd')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=3,layout.pos.col = 2))
  plotBarPlotbyAge(DATA = hhsizebyage_rural[[countries[co]]],INDEX = 'e')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=3,layout.pos.col = 3))
  plotBarPlotbyAge(DATA = hhsizebyage[[countries[co]]],INDEX = 'f')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=4,layout.pos.col = 1))
  plotPopPyramidUrban(COUNTRY = countries[co],INDEX = 'g')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=4,layout.pos.col = 2))
  plotPopPyramidRural(COUNTRY = countries[co],INDEX = 'h')
  popViewport()
  
  pushViewport(viewport(layout.pos.row=4,layout.pos.col = 3))
  plotPopPyramid(COUNTRY = countries[co],INDEX = 'i')
  popViewport()
  
  popViewport()
  popViewport()
  # dev.off()
}
dev.off()
