# ## Compare the relevant urban/rural matrices to the empirical matrices 
URBANRURAL_ANALYSIS = TRUE
# ## Compare the 2017 matrices to the empirical matrices 
COMPARE_2017_ANALYSIS = FALSE
source('codes/2_normalise.r')


isolist = names(empirical)
rowlist = rep(1:5,2)
agegplist = c(5,5,5,1,5,5,5,1,1,5)
agelabellist = list(CHN='',
                    FRA='',
                    HKG='',
                    KEN=c('0-5','6-15','16-20','21-50','51+'),
                    PER='',
                    RUS='',
                    ZAF='',
                    UGA=c('0-5','6-10','11-15','16-25','26-35','36-45','46-55','56-65','66+'),
                    VNM=c('0-5','6-15','16-25','26-34','35-49','50-64','65+'),
                    ZWE='')
nage = as.numeric(lapply(empirical, function(x) nrow(x$all)))

correlations = array(NA,length(empirical))
for(i in 1:length(correlations))
{
  correlations[i] = cor(as.vector(empirical[[i]]$all),as.vector(synthetic2020[[i]]$all))
}
summary(correlations)


if(SAVEPLOT) png('plots/compare_empirical_synthetic_urbanrural.png',height=30,width=30/5*3*2,units='cm',res=1000,pointsize=10)
if(1)
{
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2,width=c(rep(1,2)),height=c(rep(1,2)))))
  
  pushViewport(viewport(layout.pos.col=1))
  pushViewport(plotViewport(c(4.5,3,1.5,0.5)))
  
  pushViewport(viewport(layout=grid.layout(nrow=5,ncol=3,width=c(rep(1,5)),height=c(rep(1,6)))))
  
  for(ro in 1:5)
  {
    if(rowlist[ro] == 1) 
    {
      pushViewport(viewport(layout.pos.col=1,layout.pos.row=rowlist[ro]))
      grid.text('Empirical contact survey',y=unit(1,'npc')+unit(0.75,'lines'),rot=0,gp=gpar(fontsize=10,fontface='bold'))
      grid.text('normalised',y=unit(1,'npc')+unit(0.0,'lines'),rot=0,gp=gpar(fontsize=10,fontface='italic'))
      popViewport()
      
      pushViewport(viewport(layout.pos.col=2,layout.pos.row=rowlist[ro]))
      grid.text('Synthetic 2020',y=unit(1,'npc')+unit(0.75,'lines'),rot=0,gp=gpar(fontsize=10,fontface='bold'))
      grid.text('normalised',y=unit(1,'npc')+unit(0.0,'lines'),rot=0,gp=gpar(fontsize=10,fontface='italic'))
      popViewport()
    }
    
    pushViewport(viewport(layout.pos.col=1,layout.pos.row=rowlist[ro]))
    iso =isolist[ro]
    if(iso %in% "CHN") grid.text('Shanghai, China',x=unit(-2.85,'lines'),rot=90,gp=gpar(fontsize=9,fontface='bold'))
    if(iso %in% "HKG") grid.text('Hong Kong SAR, China',x=unit(-2.85,'lines'),rot=90,gp=gpar(fontsize=9,fontface='bold'))
    if(!(iso %in% "CHN")&!(iso %in% "HKG")) grid.text(poptotal$countryname[poptotal$iso3c %in% iso],x=unit(-2.85,'lines'),rot=90,gp=gpar(fontsize=9,fontface='bold'))
    cols <- colour_blue(empirical[[iso]]$all,max.value = 1.0)
    plotContact(cols=cols,LOCATION = '',INDEX = '',YLAB = TRUE,XLAB = ifelse(rowlist[ro] == 5,TRUE,FALSE),a = agegplist[ro],AGELABEL = agelabellist[[ro]],nage = nage[ro])
    popViewport()
    
    pushViewport(viewport(layout.pos.col=2,layout.pos.row=rowlist[ro]))
    cols <- colour_blue(synthetic2020[[iso]]$all,max.value = 1.0)
    plotContact(cols=cols,LOCATION = '',INDEX = '',YLAB = FALSE,XLAB = ifelse(rowlist[ro] == 5,TRUE,FALSE),a = agegplist[ro],AGELABEL = agelabellist[[ro]],nage = nage[ro])
    popViewport()
    
    pushViewport(viewport(layout.pos.col=3,layout.pos.row=rowlist[ro]))
    plotCorrelation(ISO = iso,EMPIRICAL = empirical[[iso]]$all, MODELLED = synthetic2020[[iso]]$all,SCALE = 0.5)
    popViewport()
    
    
    if(rowlist[ro] == 5) 
    {
      pushViewport(viewport(layout.pos.col=1,layout.pos.row=5))
      numberlegend = seq(0,1,0.01)
      colourlegend = colour_blue(numberlegend,1) 
      xposlegend = (seq(0.15,0.85,length.out = length(numberlegend)+1))
      yposlegend = c(0.15,0.2)
      for(i in 1:length(numberlegend)) grid.polygon(x=unit(c(xposlegend[i],xposlegend[i+1],xposlegend[i+1],xposlegend[i]),'npc'),
                                                    y=unit(c(yposlegend[1],yposlegend[1],yposlegend[2],yposlegend[2]),'npc')-unit(4.80,'lines'),
                                                    gp = gpar(fill=colourlegend[i],col=colourlegend[i]))
      grid.polygon(y=unit(c(yposlegend[1],yposlegend[1],yposlegend[2],yposlegend[2]),'npc')-unit(4.80,'lines'),
                   x=unit(c(max(xposlegend),min(xposlegend),min(xposlegend),max(xposlegend)),'npc'),
                   gp = gpar(fill=NA,col='black'))
      grid.text('Nomalised mean number of contacts',x=unit(0.5,'npc'),y=unit(-5,'lines'),default.units = 'npc',rot = 0,gp=gpar(fontsize=8,fontface='bold'))
      grid.text(seq(0,1,0.25),x=unit(xposlegend[match(seq(0,1,0.25),numberlegend)],'npc'),y=unit(-4.0,'lines'), gp=gpar(fontsize=8,fontface='plain'))
      popViewport()
    }
    
  }
  popViewport()
  popViewport()
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2))
  pushViewport(plotViewport(c(4.5,3,1.5,0.5)))
  
  pushViewport(viewport(layout=grid.layout(nrow=5,ncol=3,width=c(rep(1,5)),height=c(rep(1,6)))))
  
  for(ro in 6:10)
  {
    if(rowlist[ro] == 1) 
    {
      pushViewport(viewport(layout.pos.col=1,layout.pos.row=rowlist[ro]))
      grid.text('Empirical contact survey',y=unit(1,'npc')+unit(0.75,'lines'),rot=0,gp=gpar(fontsize=10,fontface='bold'))
      grid.text('normalised',y=unit(1,'npc')+unit(0.0,'lines'),rot=0,gp=gpar(fontsize=10,fontface='italic'))
      popViewport()
      
      pushViewport(viewport(layout.pos.col=2,layout.pos.row=rowlist[ro]))
      grid.text('Synthetic 2020',y=unit(1,'npc')+unit(0.75,'lines'),rot=0,gp=gpar(fontsize=10,fontface='bold'))
      grid.text('normalised',y=unit(1,'npc')+unit(0.0,'lines'),rot=0,gp=gpar(fontsize=10,fontface='italic'))
      popViewport()
    }
    
    pushViewport(viewport(layout.pos.col=1,layout.pos.row=rowlist[ro]))
    iso =isolist[ro]
    grid.text(poptotal$countryname[poptotal$iso3c %in% iso],x=unit(-2.85,'lines'),rot=90,gp=gpar(fontsize=9,fontface='bold'))
    cols <- colour_blue(empirical[[iso]]$all,max.value = 1.0)
    plotContact(cols=cols,LOCATION = '',INDEX = '',YLAB = TRUE,XLAB = ifelse(rowlist[ro] == 5,TRUE,FALSE),a = agegplist[ro],AGELABEL = agelabellist[[ro]],nage = nage[ro])
    popViewport()
    
    pushViewport(viewport(layout.pos.col=2,layout.pos.row=rowlist[ro]))
    cols <- colour_blue(synthetic2020[[iso]]$all,max.value = 1.0)
    plotContact(cols=cols,LOCATION = '',INDEX = '',YLAB = FALSE,XLAB = ifelse(rowlist[ro] == 5,TRUE,FALSE),a = agegplist[ro],AGELABEL = agelabellist[[ro]],nage = nage[ro])
    popViewport()
    
    pushViewport(viewport(layout.pos.col=3,layout.pos.row=rowlist[ro]))
    plotCorrelation(ISO = iso,EMPIRICAL = empirical[[iso]]$all, MODELLED = synthetic2020[[iso]]$all,SCALE = 0.5)
    popViewport()
    
  
  }
  popViewport()
  popViewport()
  popViewport()
  
  
}
if(SAVEPLOT) dev.off()



