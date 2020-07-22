library(grid)
library(grDevices)
library(gtools)
library(countrycode)
cm=1/2.54

##### DATA

# load in the data 
load('output/hampolymoddhs.rdata')
load('output/hamworld.rdata')
load('input/pop/popratio.rdata')

# Country codes 
POLYMODandDHscountries = names(HAM_POLYMODandDHS)


source('codes/contactmatricesworld_function.r')
source('codes/contactmatricesplot_function.r')

correlationHAM = array(NA, length(POLYMODandDHscountries)) 




pdf(paste0('validation/validated_bootstrap.pdf'),height=9*cm,width=23*cm)
for(co in 1:length(POLYMODandDHscountries)) {
  # png(paste0('validation/validated_bootstrap_',countrieslowercase[c],'.png'),height=13,width=18,units = 'cm' ,res=700,pointsize=10)
  empirical =HAM_POLYMODandDHS[[POLYMODandDHscountries[co]]][1:16,1:16]
  modelled = HAM_WORLD[[POLYMODandDHscountries[co]]]
  max_home = max(empirical,modelled)+0.05
  correlationHAM[co] = cor(as.vector(empirical),as.vector(modelled))
  # png(paste0('validation/validated_bootstrap_',countrieslowercase[c],'.png'),height=9,width=23,units='cm',res=700,pointsize=10)
  grid.newpage()
  pushViewport(plotViewport(c(1,1,1,1)))
  
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=3,width=c(1,1,1),height=c(0.1,1))))
  pushViewport(viewport(layout.pos.row=1))
  countryname = popratio$countryname[popratio$iso3c %in% POLYMODandDHscountries[co]]
  grid.text(paste('HAM of',countryname),gp=gpar(fontsize=14,fontface='bold'),y=.7)
  
  popViewport()
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 1))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(empirical, max.value = max_home),
                       LOCATION = 'Empirical POLYMOD / DHS data',a=5,INDEX = 'a',CONTACT = FALSE)
  popViewport()
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 2))
  getPlot.contact.full(nrow=16,ncol=16,cols=colour_blue(modelled, max.value = max_home),
                       LOCATION = 'Modelled HAM',a=5,INDEX = 'b',CONTACT = FALSE)
  popViewport()
  pushViewport(viewport(layout.pos.row=2,layout.pos.col = 3))
  getCorrelationPlot(MAX=max_home,INDEX = 'c',EMPIRICAL = empirical,MODELLED = modelled)
  popViewport()
  
  
  pushViewport(viewport(layout.pos.row=1,layout.pos.col = 3))
  numberlegend = seq(0,max_home,0.01)
  colourlegend = colour_blue(numberlegend,max_home) 
  xposlegend = (seq(0.15,0.85,length.out = length(numberlegend)+1))
  yposlegend = c(0.65,1.0)
  for(i in 1:length(numberlegend)) grid.polygon(x=unit(c(xposlegend[i],xposlegend[i+1],xposlegend[i+1],xposlegend[i]),'npc'),
                                                y=unit(c(yposlegend[1],yposlegend[1],yposlegend[2],yposlegend[2]),'npc')+unit(0,'lines'),
                                                gp = gpar(fill=colourlegend[i],col=colourlegend[i]))
  grid.polygon(y=unit(c(yposlegend[1],yposlegend[1],yposlegend[2],yposlegend[2]),'npc')+unit(0,'lines'),
               x=unit(c(max(xposlegend),min(xposlegend),min(xposlegend),max(xposlegend)),'npc'),
               gp = gpar(fill=NA,col='black'))
  grid.text('Mean number of household members\nin a specific age group',x=unit(0.5,'npc'),y=unit(-0.5,'lines'),default.units = 'npc',rot = 0,gp=gpar(fontsize=7,fontface='bold'))
  if(max_home <1.5) grid.text(seq(0,(max_home %/%0.25)*0.25,0.25),x=unit(xposlegend[match(seq(0,(max_home %/%0.25)*0.25,0.25),numberlegend)],'npc'),y=unit(1.0,'lines'), gp=gpar(fontsize=7,fontface='plain'))
  if(max_home >=1.5) grid.text(seq(0,(max_home %/%0.5)*0.5,0.5),x=unit(xposlegend[match(seq(0,(max_home %/%0.5)*0.5,0.5),numberlegend)],'npc'),y=unit(1.0,'lines'), gp=gpar(fontsize=7,fontface='plain'))
  popViewport()
  
  
  popViewport()
  popViewport()
  # dev.off()
}
dev.off()

summary(correlationHAM)


popage=piage=piageactual= list()

for(co in 1:length(POLYMODandDHscountries)){
  popage[[POLYMODandDHscountries[co]]] = as.numeric(popratio[popratio$iso3c %in% POLYMODandDHscountries[co],paste0('age',seq(0,75,5))][1,])
}

for(co in 1:length(POLYMODandDHscountries)){
  piage[[POLYMODandDHscountries[co]]]  = rowSums(popage[[POLYMODandDHscountries[co]]] * HAM_WORLD[[POLYMODandDHscountries[co]]])
}

for(co in 1:length(POLYMODandDHscountries)){
  piageactual[[POLYMODandDHscountries[co]]]  = rowSums(popage[[POLYMODandDHscountries[co]]] * HAM_POLYMODandDHS[[POLYMODandDHscountries[co]]][1:16,1:16])
}

lapply(popage, range)
lapply(piage, range)


pdf(paste0('validation/validated_popagestructure.pdf'),height=9*cm,width=23*cm)
for(co in 1:length(POLYMODandDHscountries)){
  # png(paste0('validation/validated_popagestructure_',countrieslowercase[c],'.png'),height=9,width=23,units='cm',res=700,pointsize=10)
 
  grid.newpage()
  pushViewport(plotViewport(c(1.1,2.5,1,1)))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=3,width=c(1.05,1,1),height=c(0.1,1))))
  
  pushViewport(viewport(layout.pos.row = 1))
  country = countrycode(POLYMODandDHscountries[co],origin = 'iso3c',destination = 'country.name')
  grid.text(country,gp=gpar(fontsize = 15,fontface = 'bold'))
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 2,layout.pos.col=1))
  pushViewport(plotViewport(c(2.5,1.1,1,1),xscale=c(0,16),yscale=c(0,0.25)))
  grid.rect()
  grid.yaxis(gp=gpar(fontsize = 9))
  grid.xaxis(at = seq(0,16,2),label = seq(0,80,10), gp=gpar(fontsize = 9))
  grid.text("Proportion", x=unit(-2.5, "lines"), rot=90,gp=gpar(fontsize=12))
  grid.text("Age", y=unit(-2.5, "lines"), gp=gpar(fontsize=12))
  grid.text(expression(paste(p[a])),0.5,unit(1,'npc')+unit(-1,'lines'), gp=gpar(fontsize=14))
  for(i in 1:16) grid.polygon(x = c((i-1),i,i,(i-1)),y= c(0,0,popage[[POLYMODandDHscountries[co]]][i],popage[[POLYMODandDHscountries[co]]][i]),default.units = 'native',gp=gpar(fill='grey40',col='white',lwd=2))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
  popViewport()
  
  
  pushViewport(viewport(layout.pos.row = 2,layout.pos.col=2))
  pushViewport(plotViewport(c(2.5,1,1,1),xscale=c(0,16),yscale=c(0,(round(max(piageactual[[POLYMODandDHscountries[co]]]),1)+0.5))))
  grid.rect()
  grid.yaxis(gp=gpar(fontsize = 9))
  grid.xaxis(at = seq(0,16,2),label = seq(0,80,10), gp=gpar(fontsize = 9))
  grid.text("Age", y=unit(-2.5, "lines"), gp=gpar(fontsize=12))
  grid.text(expression(paste(pi[b]) %prop% paste(sum(p[a]*N[ab], a ))),0.5,unit(1,'npc')+unit(-1,'lines'), gp=gpar(fontsize=14))
  grid.text('Data',0.5,unit(1,'npc')+unit(1,'lines'), gp=gpar(fontsize=8))
  for(i in 1:16) grid.polygon(x = c((i-1),i,i,(i-1)),y= c(0,0,piageactual[[POLYMODandDHscountries[co]]][i],piageactual[[POLYMODandDHscountries[co]]][i]), default.units = 'native',gp=gpar(fill='grey40',col='white',lwd=2))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
  popViewport()
  
  
  pushViewport(viewport(layout.pos.row = 2,layout.pos.col=3))
  pushViewport(plotViewport(c(2.5,1,1,1),xscale=c(0,16),yscale=c(0,(round(max(piage[[POLYMODandDHscountries[co]]]),1)+0.5))))
  grid.rect()
  grid.yaxis(gp=gpar(fontsize = 9))
  grid.xaxis(at = seq(0,16,2),label = seq(0,80,10), gp=gpar(fontsize = 9))
  grid.text("Age", y=unit(-2.5, "lines"), gp=gpar(fontsize=12))
  grid.text(expression(paste(widehat(pi[b])) %prop% paste(sum(p[a]*widehat(N[ab]), a ))),0.5,unit(1,'npc')+unit(-1,'lines'), gp=gpar(fontsize=14))
  grid.text('Validated',0.5,unit(1,'npc')+unit(1,'lines'), gp=gpar(fontsize=8))
  for(i in 1:16) grid.polygon(x = c((i-1),i,i,(i-1)),y= c(0,0,piage[[POLYMODandDHscountries[co]]][i],piage[[POLYMODandDHscountries[co]]][i]), default.units = 'native',gp=gpar(fill='grey40',col='white',lwd=2))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
  popViewport()
  # dev.off()
}
dev.off()


correlation = array(NA, length(POLYMODandDHscountries))
for (co in 1:length(POLYMODandDHscountries))
{
  empirical = HAM_POLYMODandDHS[[POLYMODandDHscountries[co]]][1:16,1:16]
  modelled = HAM_WORLD[[POLYMODandDHscountries[co]]]
  correlation[co] =  (cor(as.vector(empirical),as.vector(modelled)))
  rm(empirical,modelled)
}

quantile(correlation)
