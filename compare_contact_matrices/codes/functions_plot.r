require(grid)
require(countrycode)
require(viridis)


plotContact = function(cols,LOCATION,INDEX,YLAB=FALSE,XLAB=FALSE,a=5,nage =16,AGELABEL=''){
  nrow = ncol = nage
  pushViewport(plotViewport(c(0.5,0.5,0.5,0.5)*1.5,xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect(gp=gpar(fill='grey'))
  if(a==5)grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  if(a==5)grid.yaxis(at=(0:ncol)*a,label=F,gp=gpar(fontsize=3))
  if(a==5)grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=F,gp=gpar(fontsize=8))
  if(a==5)grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  if(a!=5) grid.xaxis(at=seq(0,nrow-1,by=1)+0.5,label= AGELABEL,gp=gpar(fontsize=4.5))
  
  if(a==5&!YLAB) grid.yaxis(at=seq(0,ncol/2,by=1)*(a*2),label=F,gp=gpar(fontsize=8))
  if(a==5&YLAB) grid.yaxis(at=seq(0,ncol/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  if(a!=5&!YLAB) grid.yaxis(at=seq(0,ncol-1,by=1)+0.5,label= F,gp=gpar(fontsize=6))
  if(a!=5&YLAB) grid.yaxis(at=seq(0,ncol-1,by=1)+0.5,label= AGELABEL,gp=gpar(fontsize=6))
  if(XLAB) grid.text('Age of individual',y=unit(-2.0,'lines'),gp=gpar(fontsize=9,fontface='bold'))
  if(YLAB) grid.text('Age of contact',x=unit(-2.5,'lines'),rot=90,gp=gpar(fontsize=9,fontface='bold'))
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  grid.text(paste(LOCATION),0.5,unit(1,'npc')+unit(-0.8,'lines'), gp=gpar(fontsize=8.5))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=9,col=NA))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

colour_blue =  function(contactmatrix,max.value){
  colours = colorRampPalette(c("white","steelblue", "navy", 'black'),bias=2.0) 
  z = contactmatrix
  ncol=100
  allcol=colours(ncol)
  i=ceiling((z*ncol/max.value)^1)
  i=i*(i>0)+1*(i==0)
  cols=allcol[i]
}
colour =  function(contactmatrix,max.value){
  colours = colorRampPalette(viridis(n = 20),bias=2.5)
  z = contactmatrix
  ncol=100
  allcol=colours(ncol)
  i=ceiling((z*ncol/max.value)^1)
  i=i*(i>0)+1*(i==0)
  cols=allcol[i]
}


plotStep = function(c1,c2,a=5,LOCATION,INDEX,AGELABEL='',YLAB=TRUE,XLAB=FALSE,YMAX=10,LEGEND=FALSE){
  nrow = ncol = length(c1)
  pushViewport(plotViewport(c(0.5,1.0,0.5,0.5)*1.5,xscale=c(0,nrow*a),yscale=c(0,YMAX)))
  grid.rect(gp=gpar(fill='white'))
  if(a==5) grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  if(a==5)grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=F,gp=gpar(fontsize=8))
  if(a==5)grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  if(a!=5) grid.xaxis(at=(seq(0,nrow-1,by=1)+0.5),label=unlist(AGELABEL),gp=gpar(fontsize=6))
  
  if(YLAB) grid.yaxis(label=T,gp=gpar(fontsize=6))
  if(XLAB) grid.text('Age of individual',y=unit(-2.0,'lines'),gp=gpar(fontsize=9,fontface='bold'))
  if(YLAB) grid.text('Mean #contacts (normalised)',x=unit(-2.05,'lines'),rot=90,gp=gpar(fontsize=8,fontface='plain'))
  
  for(i in 1:nrow)
  {
    c1_ci = exactPoiCI(c1[i])
    c2_ci = exactPoiCI(c2[i])
    grid.polygon(x=c((i-1)*a,i*a,i*a,(i-1)*a),y=c(c1_ci[1],c1_ci[1],c1_ci[2],c1_ci[2]),default.units = 'native',gp=gpar(fill='coral',col=NA,alpha=0.3))
    grid.polygon(x=c((i-1)*a,i*a,i*a,(i-1)*a),y=c(c2_ci[1],c2_ci[1],c2_ci[2],c2_ci[2]),default.units = 'native',gp=gpar(fill='cornflowerblue',col=NA,alpha=0.3))
    grid.lines(x = c((i-1)*a,i*a),y = c(c1[i],c1[i]),default.units = 'native',gp=gpar(col='coral',lwd=1))
    grid.lines(x = c((i-1)*a,i*a),y = c(c2[i],c2[i]),default.units = 'native',gp=gpar(col='cornflowerblue',lwd=1))
    grid.lines(x = c(i*a,i*a),y = c(c1[i],c1[i+1]),default.units = 'native',gp=gpar(col='coral',lwd=1))
    grid.lines(x = c(i*a,i*a),y = c(c2[i],c2[i+1]),default.units = 'native',gp=gpar(col='cornflowerblue',lwd=1))
    
  }
  grid.text(paste(LOCATION),0.5,unit(1,'npc')+unit(-0.8,'lines'), gp=gpar(fontsize=8.5))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=9,col=NA))
  
  if(LEGEND) 
  {
    grid.polygon(x=c(0.93,0.98,0.98,0.93),y=c(0.825,0.825,0.875,0.875),default.units = 'npc',gp=gpar(fill='coral',col=NA,alpha=0.3))
    grid.polygon(x=c(0.93,0.98,0.98,0.93),y=c(0.725,0.725,0.775,0.775),default.units = 'npc',gp=gpar(fill='cornflowerblue',col=NA,alpha=0.3))
    grid.lines(x=c(0.93,0.98),y=c(0.85,0.85),default.units = 'npc',gp=gpar(col='coral',lwd=1))
    grid.lines(x=c(0.93,0.98),y=c(0.75,0.75),default.units = 'npc',gp=gpar(col='cornflowerblue',lwd=1))
    grid.text(c('Synthetic','Empirical'),x = 0.91,y = c(0.85,0.75),just = 'right',gp=gpar(fontsize=7))
  }
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

plotCorrelation = function(ISO,INDEX='',EMPIRICAL,MODELLED,XAXISNAME = 'Empirical contact survey',YAXISNAME = 'Synthetic 2020'){
  # grid.newpage()
  MAX = max(c(as.vector(EMPIRICAL),as.vector(MODELLED)),na.rm = TRUE)
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(-0.01,(MAX+0.1)),yscale=c(-0.01,(MAX+0.1))))
  grid.rect()
  grid.xaxis(gp=gpar(fontsize=6))
  grid.yaxis(gp=gpar(fontsize=6))
  
  grid.text(XAXISNAME,y=unit(-2.1,'lines'),gp=gpar(fontsize=7,fontface = 'bold'))
  grid.text(YAXISNAME,x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7,fontface='bold'))
  
  matrixagecolour = rep(seq(1,ncol(MODELLED),1),times = ncol(MODELLED))
  scaleagecolour = viridis(max(matrixagecolour))
  # scaleagecolour = 'grey40'
  
  grid.points(x=c(as.vector(EMPIRICAL)),y=c(as.vector(MODELLED)),size = unit(.5, "char"), pch=20,default.units = "native",gp=gpar(col=scaleagecolour,fill=scaleagecolour,alpha = .8))
  grid.lines(x=unit(c(0,MAX+.09),units = 'native'),y=unit(c(0,MAX+.09),units = 'native'),gp=gpar(lwd=2,lty='dashed',col='black'))
  grid.text(paste0('r = ',round(cor(as.vector(EMPIRICAL),as.vector(MODELLED),use = 'complete.obs'),2)),unit(0.5,'npc'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=8))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5,col=NA))
  ypos = seq(0.05,0.7,length.out = 16)
  allages = paste0(seq(0,75,5),'-',seq(4,79,5))
  if(length(agelabellist[[ISO]])==1) agegp = allages
  if(length(agelabellist[[ISO]])>1) agegp = agelabellist[[ISO]]
  # grid.polygon(x = c(0.89,0.99,0.99,0.89),y = c(0.72,0.72,0.03,0.03),default.units = 'npc',gp=gpar(fill='grey',col=NA))
  grid.points(x = unit(rep(1,ncol(MODELLED)),'npc')+unit(0.2,'lines'),y = unit(ypos[1:ncol(MODELLED)],'npc'),size = unit(.5, "char"), pch=20,
              gp=gpar(col=scaleagecolour[1:ncol(MODELLED)],fill=scaleagecolour[1:ncol(MODELLED)],alpha = 1))
  grid.text(agegp[1:ncol(MODELLED)],x = unit(rep(1,ncol(MODELLED)),'npc')+unit(0.75,'lines'),y = unit(ypos[1:ncol(MODELLED)],'npc'),just = 'left',gp=gpar(fontsize = 5))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

plotCorrelation = function(ISO,INDEX='',EMPIRICAL,MODELLED,XAXISNAME = 'Empirical contact survey',YAXISNAME = 'Synthetic 2020',SCALE){
  # grid.newpage()
  MAX = 1^SCALE#max(c(as.vector(EMPIRICAL)^SCALE,as.vector(MODELLED)^SCALE),na.rm = TRUE)
  pushViewport(plotViewport(c(1.5,1.5,0.75,.75),xscale=c(-0.01,(MAX)),yscale=c(-0.01,(MAX))))
  grid.rect()
  grid.xaxis(at = c(seq(0,1,0.2))^SCALE, label = seq(0,1,0.2),gp=gpar(fontsize=6))
  grid.yaxis(at = c(seq(0,1,0.2))^SCALE, label = seq(0,1,0.2),gp=gpar(fontsize=6))
  # if(MAX^(1/SCALE) < 0.8) grid.xaxis(at = c(seq(0,0.6,0.1))^SCALE, label = seq(0,0.6,0.1),gp=gpar(fontsize=6))
  # if(MAX^(1/SCALE) < 0.8) grid.yaxis(at = c(seq(0,0.6,0.1))^SCALE, label = seq(0,0.6,0.1),gp=gpar(fontsize=6))
  
  grid.text(XAXISNAME,y=unit(-2.1,'lines'),gp=gpar(fontsize=7,fontface = 'bold'))
  grid.text(YAXISNAME,x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7,fontface='bold'))
  
  matrixagecolour = rep(seq(1,ncol(MODELLED),1),times = ncol(MODELLED))
  scaleagecolour = viridis(max(matrixagecolour))
  # scaleagecolour = 'grey40'
  
  ypos = seq(0.05,0.7,length.out = 16)
  allages = paste0(seq(0,75,5),'-',seq(4,79,5))
  if(length(agelabellist[[ISO]])==1) agegp = allages
  if(length(agelabellist[[ISO]])>1) agegp = agelabellist[[ISO]]
  yvalues = ypos[1:ncol(MODELLED)]
  grid.polygon(x = c(0.79,0.99,0.99,0.79),y = c(max(yvalues)+0.03,max(yvalues)+0.03,0.02,0.02),default.units = 'npc',gp=gpar(fill='grey90',col=NA))
  grid.points(x = unit(rep(0.81,ncol(MODELLED)),'npc'),y = unit(ypos[1:ncol(MODELLED)],'npc'),size = unit(.5, "char"), pch=20,
              gp=gpar(col=scaleagecolour[1:ncol(MODELLED)],fill=scaleagecolour[1:ncol(MODELLED)],alpha = 1))
  grid.text(agegp[1:ncol(MODELLED)],x = unit(rep(0.84,ncol(MODELLED)),'npc')+unit(0,'lines'),y = unit(ypos[1:ncol(MODELLED)],'npc'),just = 'left',gp=gpar(fontsize = 5.5))
  
  grid.points(x=c(as.vector(EMPIRICAL)^SCALE),y=c(as.vector(MODELLED)^SCALE),size = unit(.5, "char"), pch=20,default.units = "native",
              gp=gpar(col=scaleagecolour[matrixagecolour],fill=scaleagecolour[matrixagecolour],alpha = .8))
  grid.lines(x=unit(c(0,MAX-0.01),units = 'native'),y=unit(c(0,MAX-0.01),units = 'native'),gp=gpar(lwd=2,lty='dashed',col='black'))
  grid.text(paste0('r = ',round(cor(as.vector(EMPIRICAL),as.vector(MODELLED),use = 'complete.obs'),2)),unit(0.5,'npc'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=8))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5,col=NA))
  
  
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

plotCorrelation_urbanrural = function(RURAL,URBAN,INDEX, XAXISNAME ,YAXISNAME,SCALE,YMAX,YMIN,TITLE,GROUP){
  # grid.newpage()
  MIN = YMIN^SCALE
  MAX = YMAX^SCALE #max(c(as.vector(EMPIRICAL)^SCALE,as.vector(MODELLED)^SCALE),na.rm = TRUE)
  pushViewport(plotViewport(c(1.5,1.5,1,1),xscale=c(MIN,(MAX)),yscale=c(MIN,(MAX))))
  grid.rect()
  grid.xaxis(gp=gpar(fontsize=7))
  grid.yaxis(gp=gpar(fontsize=7))
  # grid.xaxis(at = c(seq(0,1,0.2))^SCALE, label = seq(0,1,0.2),gp=gpar(fontsize=6))
  # grid.yaxis(at = c(seq(0,1,0.2))^SCALE, label = seq(0,1,0.2),gp=gpar(fontsize=6))
  # if(MAX^(1/SCALE) < 0.8) grid.xaxis(at = c(seq(0,0.6,0.1))^SCALE, label = seq(0,0.6,0.1),gp=gpar(fontsize=6))
  # if(MAX^(1/SCALE) < 0.8) grid.yaxis(at = c(seq(0,0.6,0.1))^SCALE, label = seq(0,0.6,0.1),gp=gpar(fontsize=6))
  
  grid.text(XAXISNAME,y=unit(-2.1,'lines'),gp=gpar(fontsize=8,fontface = 'plain'))
  grid.text(YAXISNAME,x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=8,fontface='plain'))
  
  matrixagecolour = GROUP#rep(seq(1,ncol(MODELLED),1),times = ncol(MODELLED))
  # scaleagecolour = viridis(max(matrixagecolour))
  # scaleagecolour = 'grey40'
  scaleagecolour = c('#FFAA3C','#E6447D','#652E79','#4BD4D4')
  grid.points(x=c(as.vector(RURAL)^SCALE),y=c(as.vector(URBAN)^SCALE),size = unit(.5, "char"), pch=20,default.units = "native",
              gp=gpar(col=scaleagecolour[GROUP],fill=scaleagecolour[GROUP],alpha = .8))
  grid.lines(x=unit(c(MIN+0.01,MAX-0.01),units = 'native'),y=unit(c(MIN+0.01,MAX-0.01),units = 'native'),gp=gpar(lwd=1.5,lty='dashed',col='black'))
  grid.text(TITLE,unit(0.5,'npc'),unit(1,'npc')+unit(-1,'lines'), gp=gpar(fontsize=8,fontface='bold'))
  # grid.text(paste0('r = ',round(cor(as.vector(RURAL),as.vector(URBAN),use = 'complete.obs'),2)),unit(0.5,'npc'),unit(1,'npc')+unit(-0.8,'lines'),
  #           gp=gpar(fontsize=8))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-1,'lines'),gp=gpar(fontsize=8,col='black'))
  ypos = seq(0.04,0.28,length.out = max(matrixagecolour))
  legend = c('LIC','LMIC','UMIC','HIC')
  cors = array(NA,max(matrixagecolour))
  for(g in 1:max(matrixagecolour))
  {
    cors[g] = cor(RURAL[GROUP %in% g], URBAN[GROUP %in% g])
  }

  grid.polygon(x = c(0.7,0.99,0.99,0.7),y = c(0.3,0.3,0.02,0.02),default.units = 'npc',gp=gpar(fill='grey90',col=NA))
  grid.points(x = unit(rep(0.715,max(matrixagecolour)),'npc'),y = unit(ypos[1:max(matrixagecolour)],'npc'),size = unit(.5, "char"), pch=20,
              gp=gpar(col=scaleagecolour[max(matrixagecolour):1],fill=scaleagecolour[max(matrixagecolour):1],alpha = 1))
  grid.text(paste0(legend,' (r = ', round(cors,2),')'),x = unit(rep(0.735,max(matrixagecolour)),'npc'),y = unit(rev(ypos),'npc'),just = 'left',gp=gpar(fontsize = 7))

  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

plotOtherContacts = function(ISO,a=5,LOCATION,INDEX,AGELABEL='',YLAB=TRUE,XLAB=FALSE,LEGEND=FALSE)
{
  nrow = 16
  # grid.newpage()
  pushViewport(plotViewport(c(0.5,1.0,0.75,0.5)*1.5,xscale=c(-0,nrow*a+0),yscale=c(0,1)))
  grid.rect(gp=gpar(fill='white'))
  if(a==5)grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  if(a==5)grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=F,gp=gpar(fontsize=8))
  if(a==5)grid.text(label = seq(0,nrow/2,by=1)*(a*2),x =unit(seq(0,nrow/2,by=1)*(a*2),'native'),y = unit(-1,'lines'),gp=gpar(fontsize = 8))
  # if(a==5)grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=8))
  
  grid.yaxis(label=T,gp=gpar(fontsize=8))
  if(XLAB) grid.text('Age of individual',y=unit(-2.0,'lines'),gp=gpar(fontsize=9,fontface='bold'))
  if(YLAB) grid.text('Proportion of contacts\nat other locations',x=unit(-3.25,'lines'),rot=90,gp=gpar(fontsize=9,fontface='plain'))
  
  for(i in 1:nrow)
  {
    yval1 = c(othercontacts[[ISO]]$lower[i],othercontacts[[ISO]]$upper[i])
    yval1[yval1 <0 ] = 0
    yval2 = c(othercontacts$POLYMOD$lower[i],othercontacts$POLYMOD$upper[i])
    yval2[yval2 <0 ] = 0
    grid.lines(x = c((i-1)*a+1.25,(i-1)*a+1.25),y = yval1,default.units = 'native',gp=gpar(col='black',lwd=1))
    grid.lines(x = c((i-1)*a+3.75,(i-1)*a+3.75),y = yval2,default.units = 'native',gp=gpar(col='grey60',lwd=1))
  }
  grid.points(x = (othercontacts[[ISO]]$part_age_gp[1:nrow]-1)*a+1.25,y = othercontacts[[ISO]]$mean[1:nrow],default.units = 'native',pch =16,gp=gpar(col='black',cex=0.5))
  grid.points(x = (othercontacts$POLYMOD$part_age_gp[1:nrow]-1)*a+3.75,y = othercontacts$POLYMOD$mean[1:nrow], default.units = 'native',pch =16,gp=gpar(col='grey60',cex=0.5))
  # grid.points(x = (othercontacts$POLYMOD$part_age_gp[1:nrow]-1)*a+3.75,y = rowSums(contact_others[[ISO]])/rowSums(contact_all[[ISO]]), default.units = 'native',pch =17,gp=gpar(col='red',cex=0.5))
  grid.text(paste(LOCATION),unit(1.8,'lines'),unit(1,'npc')+unit(-0.8,'lines'),just = 'left', gp=gpar(fontface = 'bold',fontsize=9))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=9,col='black'))
  
  if(LEGEND) 
  {
    grid.lines(x=(c(0.93,0.98)),y=c(0.85,0.85),default.units = 'npc',gp=gpar(col='grey60',lwd=1))
    grid.lines(x=(c(0.93,0.98)),y=c(0.95,0.95),default.units = 'npc',gp=gpar(col='black',lwd=1))
    grid.points(x=mean(c(0.93,0.98)),y=c(0.85),default.units = 'npc',pch=16,gp=gpar(col='grey60',cex=0.5))
    grid.points(x=mean(c(0.93,0.98)),y=c(0.95),default.units = 'npc',pch=16,gp=gpar(col='black',cex=0.5))
    grid.text(c('POLYMOD survey','Region survey'),x = 0.91,y = c(0.85,0.95),just = 'right',gp=gpar(fontsize=8))
  }
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

plotReduction = function(ISO,INDEX='',DATA = reduction,YAXISNAME = '% reduction in cases')
{
  # grid.newpage()
  YRANGE = c(0,1.0)
  XRANGE = c(0.5,18.5)
  pushViewport(plotViewport(c(1.5,3.5,0.75,0.75),xscale=XRANGE,yscale=YRANGE))
  grid.polygon(c(0.5,6.5,6.5,0.5),y=c(YRANGE[1],YRANGE[1],YRANGE[2],YRANGE[2]),default.units = 'native',gp=gpar(col = NA,fill='grey90'))
  grid.polygon(c(12.5,18.5,18.5,12.5),y=c(YRANGE[1],YRANGE[1],YRANGE[2],YRANGE[2]),default.units = 'native',gp=gpar(col = NA,fill='grey90'))
  grid.rect(gp=gpar(lwd = 1.5,fill=NA))
  grid.text(label = c('20% physical','50% physical','Shielding'),x= unit(c(3.5,9.5,15.5),'native'), y = unit(c(-0.65,-0.65,-0.65),'lines'),gp=gpar(fontsize=8))
  grid.text(label = c('distancing','distancing',''),x= unit(c(3.5,9.5,15.5),'native'), y = unit(c(-1.45,-1.45,-1.45),'lines'),gp=gpar(fontsize=8))
  grid.yaxis(at = seq(0,1,0.2),label = seq(0,100,20),gp=gpar(fontsize=7))
  
  # grid.text(XAXISNAME,y=unit(-2.1,'lines'),gp=gpar(fontsize=7,fontface = 'bold'))
  grid.text(YAXISNAME,x=unit(-2.7,'lines'),rot=90,gp=gpar(fontsize= 8,fontface='plain'))
  grid.text(DATA$fullname[DATA$iso %in% ISO],x=unit(-3.7,'lines'),rot=90,gp=gpar(fontsize=8,fontface='bold'))
  
  matr = c("contact_matrix_empirical","contact_matrix_empirical_adjusted","contact_matrix_synthetic_old","contact_matrix_synthetic_new_national", 
           "contact_matrix_synthetic_new_rural","contact_matrix_synthetic_new_urban")
  for(m in 1:length(matr))
  {
    m_col = which(DATA$distance20[[ISO]]$contact_matrix %in% matr[m])
    if(length(m_col)>0)
    {
      grid.lines(x = c(m,m), y =c(DATA$distance20[[ISO]]$low95[m_col],DATA$distance20[[ISO]]$high95[m_col]),default.units = 'native',
                 gp = gpar(col = COLS[m],lwd=1))
      grid.lines(x = c(m+6,m+6), y =c(DATA$distance50[[ISO]]$low95[m_col],DATA$distance50[[ISO]]$high95[m_col]),default.units = 'native',
                 gp = gpar(col = COLS[m],lwd=1))
      grid.lines(x = c(m+12,m+12), y =c(DATA$shielding[[ISO]]$low95[m_col],DATA$shielding[[ISO]]$high95[m_col]),default.units = 'native',
                 gp = gpar(col = COLS[m],lwd=1))
      extra = 0.15
      grid.polygon(x = c(m-extra,m+extra,m+extra,m-extra),
                   y =c(DATA$distance20[[ISO]]$low50[m_col],DATA$distance20[[ISO]]$low50[m_col],DATA$distance20[[ISO]]$high50[m_col],DATA$distance20[[ISO]]$high50[m_col]),
                   default.units = 'native',gp = gpar(col = NA, fill = COLS[m]))
      grid.lines(x = c(m+extra,m-extra), y =c(DATA$distance20[[ISO]]$median[m_col],DATA$distance20[[ISO]]$median[m_col]),default.units = 'native',
                 gp = gpar(col = 'grey90',lwd=1))
      grid.polygon(x = c(m-extra,m+extra,m+extra,m-extra)+6,
                   y =c(DATA$distance50[[ISO]]$low50[m_col],DATA$distance50[[ISO]]$low50[m_col],DATA$distance50[[ISO]]$high50[m_col],DATA$distance50[[ISO]]$high50[m_col]),
                   default.units = 'native',gp = gpar(col = NA, fill = COLS[m]))
      grid.lines(x = c(m+extra,m-extra)+6, y =c(DATA$distance50[[ISO]]$median[m_col],DATA$distance50[[ISO]]$median[m_col]),default.units = 'native',
                 gp = gpar(col = 'white',lwd=1))
      grid.polygon(x = c(m-extra,m+extra,m+extra,m-extra)+12,
                   y =c(DATA$shielding[[ISO]]$low50[m_col],DATA$shielding[[ISO]]$low50[m_col],DATA$shielding[[ISO]]$high50[m_col],DATA$shielding[[ISO]]$high50[m_col]),
                   default.units = 'native',gp = gpar(col = NA, fill = COLS[m]))
      grid.lines(x = c(m+extra,m-extra)+12, y =c(DATA$shielding[[ISO]]$median[m_col],DATA$shielding[[ISO]]$median[m_col]),default.units = 'native',
                 gp = gpar(col = 'grey90',lwd=1))
    }
    
    
  }
  
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

plotAttackRate = function(ISO,INDEX='',DATA = rate,YAXISNAME = 'Infection attack rate %')
{
  # grid.newpage()
  YRANGE = c(0,1.0)
  XRANGE = c(0,80)
  pushViewport(plotViewport(c(1.5,3.5,0.75,0.75),xscale=XRANGE,yscale=YRANGE))
  for(extra in seq(0,70,10)) grid.polygon(c(0,5,5,0)+extra,y=c(YRANGE[1],YRANGE[1],YRANGE[2],YRANGE[2]),default.units = 'native',gp=gpar(col = NA,fill='grey90'))
  
  grid.rect(gp=gpar(lwd = 1.5,fill=NA))
  grid.yaxis(at = seq(0,1,0.2),label = seq(0,100,20),gp=gpar(fontsize=7))
  grid.xaxis(at = seq(0,80,5),label = FALSE,gp=gpar(fontsize=3))
  grid.xaxis(at = seq(0,80,10),label = FALSE,gp=gpar(fontsize=7))
  grid.text(seq(0,80,10),x=unit(seq(0,80,10),'native'),y = unit(-1,'lines'),gp=gpar(fontsize=7))
  
  grid.text('Age',y=unit(-2.1,'lines'),gp=gpar(fontsize=7,fontface = 'bold'))
  grid.text(YAXISNAME,x=unit(-2.7,'lines'),rot=90,gp=gpar(fontsize= 8,fontface='plain'))
  grid.text(DATA$fullname[DATA$iso %in% ISO],x=unit(-3.7,'lines'),rot=90,gp=gpar(fontsize=8,fontface='bold'))
  xextra = seq(0,5,length.out = 8)
  xextra = xextra[2:7]
  width = 0.25
  for(m in 1:6)
  {
    m_col = which(DATA$unmitigated[[ISO]]$cols %in% m)
    if(length(m_col)>0)
    {
      for(a in seq(0,75,5))
      {
        yval = which(DATA$unmitigated[[ISO]]$cols %in% m & DATA$unmitigated[[ISO]]$age %in% a)
        grid.lines(x = c(a,a)+xextra[m],y =1-c(DATA$unmitigated[[ISO]]$low95[yval],DATA$unmitigated[[ISO]]$high95[yval]),default.units = 'native',gp = gpar(col = COLS[m],lwd=1))
        grid.polygon(x = c(a-width,a+width,a+width,a-width)+xextra[m],
                     y =1-c(DATA$unmitigated[[ISO]]$low50[yval],DATA$unmitigated[[ISO]]$low50[yval],DATA$unmitigated[[ISO]]$high50[yval],DATA$unmitigated[[ISO]]$high50[yval]),
                     default.units = 'native',gp = gpar(fill = COLS[m],col=NA))
        grid.lines(x = c(a-width,a+width)+xextra[m],y =1-c(DATA$unmitigated[[ISO]]$median[yval],DATA$unmitigated[[ISO]]$median[yval]),default.units = 'native',
                   gp = gpar(col = 'white',lwd=0.25))
      }
      
    }
  }
  
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}
