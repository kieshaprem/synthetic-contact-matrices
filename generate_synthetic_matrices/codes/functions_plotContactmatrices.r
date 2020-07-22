library(grid)
library(viridis)
cm=1/2.54

plotPopPyramid = function(COUNTRY,INDEX){
  pf = as.numeric(popratio_female[popratio_female$iso3c %in% COUNTRY,4:24])*100
  pm = as.numeric(popratio_male[popratio_male$iso3c %in% COUNTRY,4:24])*100
  # grid.newpage()
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(-16,16),yscale=c(0,20)))
  grid.rect()
  for(i in 1:20) {
    ma = pm[i]
    fe = pf[i]
    grid.polygon(x=c(0,-ma,-ma,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='cornflowerblue'))
    grid.polygon(x=c(0,fe,fe,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='tomato2'))
  }
  # Axes
  agenames = seq(0,100,10)
  temp = rbind(seq(-15,15,1),c(seq(15,0,-1),seq(1,15,1)))
  xaxisnames = temp[,temp[2,]%%5==0]
  grid.xaxis(at=xaxisnames[1,],label=xaxisnames[2,],gp=gpar(fontsize=6))
  grid.yaxis(at=((0:10)*2),label=agenames,gp=gpar(fontsize=6))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text("Age", x=unit(-2.5, "lines"), rot=90,gp=gpar(fontsize=6.5))
  grid.text("Proportion of population (%)", y=unit(-2.15, "lines"), gp=gpar(fontsize=6.5))
  grid.text('Male',  unit(0,'npc')+unit(0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'left', gp=gpar(fontsize=6.5))
  grid.text('Female',  unit(1,'npc')+unit(-0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'right', gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
}
# grid.newpage()
# plotPopPyramid(COUNTRY = 'AFG',INDEX = 'a')

plotPopPyramidUrban = function(COUNTRY,INDEX){
  pf = as.numeric(popUrban$ratio_female[popUrban$ratio_female$iso3c %in% COUNTRY,4:24])*100
  pm = as.numeric(popUrban$ratio_male[popUrban$ratio_male$iso3c %in% COUNTRY,4:24])*100
  # grid.newpage()
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(-16,16),yscale=c(0,20)))
  grid.rect()
  for(i in 1:20) {
    ma = pm[i]
    fe = pf[i]
    grid.polygon(x=c(0,-ma,-ma,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='cornflowerblue'))
    grid.polygon(x=c(0,fe,fe,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='tomato2'))
  }
  # Axes
  agenames = seq(0,100,10)
  temp = rbind(seq(-15,15,1),c(seq(15,0,-1),seq(1,15,1)))
  xaxisnames = temp[,temp[2,]%%5==0]
  grid.xaxis(at=xaxisnames[1,],label=xaxisnames[2,],gp=gpar(fontsize=6))
  grid.yaxis(at=((0:10)*2),label=agenames,gp=gpar(fontsize=6))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text("Age", x=unit(-2.5, "lines"), rot=90,gp=gpar(fontsize=6.5))
  grid.text("Proportion of urban population (%)", y=unit(-2.15, "lines"), gp=gpar(fontsize=6.5))
  grid.text('Male',  unit(0,'npc')+unit(0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'left', gp=gpar(fontsize=6.5))
  grid.text('Female',  unit(1,'npc')+unit(-0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'right', gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
}
plotPopPyramidRural = function(COUNTRY,INDEX){
  pf = as.numeric(popRural$ratio_female[popRural$ratio_female$iso3c %in% COUNTRY,4:24])*100
  pm = as.numeric(popRural$ratio_male[popRural$ratio_male$iso3c %in% COUNTRY,4:24])*100
  # grid.newpage()
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(-16,16),yscale=c(0,20)))
  grid.rect()
  for(i in 1:20) {
    ma = pm[i]
    fe = pf[i]
    grid.polygon(x=c(0,-ma,-ma,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='cornflowerblue'))
    grid.polygon(x=c(0,fe,fe,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='tomato2'))
  }
  # Axes
  agenames = seq(0,100,10)
  temp = rbind(seq(-15,15,1),c(seq(15,0,-1),seq(1,15,1)))
  xaxisnames = temp[,temp[2,]%%5==0]
  grid.xaxis(at=xaxisnames[1,],label=xaxisnames[2,],gp=gpar(fontsize=6))
  grid.yaxis(at=((0:10)*2),label=agenames,gp=gpar(fontsize=6))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text("Age", x=unit(-2.5, "lines"), rot=90,gp=gpar(fontsize=6.5))
  grid.text("Proportion of rural population (%)", y=unit(-2.15, "lines"), gp=gpar(fontsize=6.5))
  grid.text('Male',  unit(0,'npc')+unit(0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'left', gp=gpar(fontsize=6.5))
  grid.text('Female',  unit(1,'npc')+unit(-0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'right', gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
}
plot_poppyramid = function(COUNTRY,INDEX){
  pyramid = data[data$country==COUNTRY,]
  pyramid[is.na(pyramid)] = 0
  pushViewport(plotViewport(c(1.6,2, 1,.8),xscale=c(0,22),yscale=c(0,20)))
  grid.rect()
  for(i in 1:19) {
    grid.rect(x=0.5,y=0.05*(i-1), height=0.05, width=pyramid[2,(i+3)]*100/44,just=c("right", "bottom"),gp=gpar(col='white',fill='cornflowerblue'))
    grid.rect(x=0.5,y=0.05*(i-1), height=0.05, width=pyramid[3,(i+3)]*100/44,just=c("left", "bottom"),gp=gpar(col='white',fill='tomato2'))
  }
  # Axes
  xaxis.text = c(seq(22,0,-2),seq(2,22,2))
  agenames = seq(0,100,10)
  temp = rbind(seq(0,22,0.5),c(seq(22,0,-1),seq(1,22,1)))
  xaxisnames = temp[,temp[2,]%%5==0]
  grid.xaxis(at=xaxisnames[1,],label=xaxisnames[2,],gp=gpar(fontsize=6))
  grid.yaxis(at=((0:10)*2),label=agenames,gp=gpar(fontsize=6))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text("Age", x=unit(-2.45, "lines"), rot=90,gp=gpar(fontsize=6.5))
  grid.text("Proportion of Population (%)", y=unit(-2.15, "lines"), gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
}

plot_poppyramid = function(COUNTRY,INDEX,DATA){
  data = DATA
  pyramid = data[data$country==COUNTRY,]
  pyramid[is.na(pyramid)] = 0
  countrytotal = sum(as.numeric(pyramid[1,-c(1:3)]))
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(-16,16),yscale=c(0,20)))
  grid.rect()
  for(i in 1:19) {
    ma = as.integer(pyramid[2,(i+3)])/countrytotal*100
    fe = as.integer(pyramid[3,(i+3)])/countrytotal*100
    grid.polygon(x=c(0,-ma,-ma,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='cornflowerblue'))
    grid.polygon(x=c(0,fe,fe,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='tomato2'))
    }
  # Axes
  agenames = seq(0,100,10)
  temp = rbind(seq(-15,15,1),c(seq(15,0,-1),seq(1,15,1)))
  xaxisnames = temp[,temp[2,]%%5==0]
  grid.xaxis(at=xaxisnames[1,],label=xaxisnames[2,],gp=gpar(fontsize=6))
  grid.yaxis(at=((0:10)*2),label=agenames,gp=gpar(fontsize=6))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text("Age", x=unit(-2.5, "lines"), rot=90,gp=gpar(fontsize=6.5))
  grid.text("Proportion of population (%)", y=unit(-2.15, "lines"), gp=gpar(fontsize=6.5))
  grid.text('Male',  unit(0,'npc')+unit(0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'left', gp=gpar(fontsize=6.5))
  grid.text('Female',  unit(1,'npc')+unit(-0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'right', gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
}

plot_poppyramid_data = function(COUNTRY,INDEX,DATA){
  data = DATA
  pyramid = data[data$country==COUNTRY,]
  pyramid[is.na(pyramid)] = 0
  countrytotal = sum(as.numeric(pyramid[1,-c(1:3)]))
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(-16,16),yscale=c(0,20)))
  grid.rect()
  for(i in 1:19) {
    ma = as.integer(pyramid[2,(i+3)])/countrytotal*100
    fe = as.integer(pyramid[3,(i+3)])/countrytotal*100
    grid.polygon(x=c(0,-ma,-ma,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='cornflowerblue'))
    grid.polygon(x=c(0,fe,fe,0),y=c(i-1,i-1,i,i), default.units = 'native',gp=gpar(col='white',fill='tomato2'))
  }
  # Axes
  agenames = seq(0,100,10)
  temp = rbind(seq(-15,15,1),c(seq(15,0,-1),seq(1,15,1)))
  xaxisnames = temp[,temp[2,]%%5==0]
  grid.xaxis(at=xaxisnames[1,],label=xaxisnames[2,],gp=gpar(fontsize=6))
  grid.yaxis(at=((0:10)*2),label=agenames,gp=gpar(fontsize=6))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.text("Age", x=unit(-2.5, "lines"), rot=90,gp=gpar(fontsize=6.5))
  grid.text("Proportion of population (%)", y=unit(-2.15, "lines"), gp=gpar(fontsize=6.5))
  grid.text('Male',  unit(0,'npc')+unit(0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'left', gp=gpar(fontsize=6.5))
  grid.text('Female',  unit(1,'npc')+unit(-0.3,'lines'),unit(0,'npc')+unit(1,'lines'),just = 'right', gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(fill=NA))
  popViewport()
}

getPlot.hhage = function(nrow,ncol,cols,country,a,INDEX){
  pushViewport(plotViewport(c(1.6,2,1,.8),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  grid.yaxis(at=(0:ncol)*a,label=F,gp=gpar(fontsize=3))
  grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  grid.yaxis(at=seq(0,ncol/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  grid.text('Age of individual',y=unit(-2.1,'lines'),gp=gpar(fontsize=7))
  grid.text('Age of HH Member',x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7))
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

## Inferred HH contacts  
getPlot.contact = function(nrow,ncol,cols,a,country,INDEX){
  pushViewport(plotViewport(c(1.6,2,1,.8),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  grid.yaxis(at=(0:ncol)*a,label=F,gp=gpar(fontsize=3))
  grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  grid.yaxis(at=seq(0,ncol/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  grid.text('Age of individual',y=unit(-2.1,'lines'),gp=gpar(fontsize=7))
  grid.text('Age of contact',x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7))
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

getPlot.contact.full = function(nrow,ncol,cols,a,LOCATION,INDEX, CONTACT = TRUE,FONTSIZEPLUS = 0){
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6+FONTSIZEPLUS))
  grid.yaxis(at=seq(0,ncol/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6+FONTSIZEPLUS))
  
  if(CONTACT) grid.text('Age group of individual',y=unit(-2.1,'lines'),gp=gpar(fontsize=7+FONTSIZEPLUS))
  if(CONTACT) grid.text('Age group of contact',x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7+FONTSIZEPLUS))
  if(!CONTACT) grid.text('Age of household member',y=unit(-2.1,'lines'),gp=gpar(fontsize=7+FONTSIZEPLUS))
  if(!CONTACT) grid.text('Age of household member',x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7+FONTSIZEPLUS))
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  if(CONTACT) grid.text(paste(LOCATION),0.5,unit(1,'npc')+unit(-0.8,'lines'), gp=gpar(fontsize=8.5+FONTSIZEPLUS,col='black'))
  if(!CONTACT) grid.text(paste(LOCATION),0.5,unit(1,'npc')+unit(0.5,'lines'), gp=gpar(fontsize=8.5+FONTSIZEPLUS,col='black'))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5+FONTSIZEPLUS,col='black'))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

getPlot.contactBARE = function(nrow,ncol,cols,a,country,INDEX){
  #   grid.newpage()
  pushViewport(plotViewport(c(1,1,1,.8),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  grid.xaxis(at=(0:nrow)*a,label=F,gp=gpar(fontsize=3))
  grid.yaxis(at=(0:ncol)*a,label=F,gp=gpar(fontsize=3))
  grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  grid.yaxis(at=seq(0,ncol/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  #   grid.text(paste('Age-specific School Contacts for',country),
  #             0.5,unit(1.1,'npc')+unit(-0.2,'lines'), gp=gpar(fontsize=8))
  #   grid.text('Age group of Individual',y=unit(-2.1,'lines'),gp=gpar(fontsize=7))
  #   grid.text('Age group of Contact',x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7))
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
} 



oldcolor =  function(KAPPA,max.value,actual){
  if(actual == T) alex.colors = colorRampPalette(c("white","steelblue", "navy", 'black'),bias=2) 
  if(actual == F)  alex.colors = colorRampPalette(c("white","#FFCC99","orange",  "darkorange", "darkorange4","black"),bias=2) 
  z = KAPPA
  ncol=100
  allcol=alex.colors(ncol)
  i=ceiling(z*ncol/max.value)
  i=i*(i>0)+1*(i==0)
  cols=allcol[i]
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
  colours = colorRampPalette(viridis(n = 20),bias=1.5)
  z = contactmatrix
  ncol=100
  allcol=colours(ncol)
  i=ceiling((z*ncol/max.value)^1)
  i=i*(i>0)+1*(i==0)
  cols=allcol[i]
}

getbeta = function(lcalculate_transmission_probability=1,npop,f,C,gamma=1,R0)
{
  # 1) lcalculate_transmission_probability if this is 1, then calculate the transmission probability from R0 otherwise, assume it is beta=0.05 
  # 2) npop = population size 
  # 3) f = population age proportion 
  # 4) C = contact matrix
  # 5) gamma = removal rate  
  # 6) R0
  
  N =  npop*f     # number in each age group
  
  nage = length(f)
  I_0    = rep(1,nage) # put one infected person in each age 
  S_0    = N-I_0
  R_0    = rep(0,nage)
  
  if (lcalculate_transmission_probability==1){
    M = C
    for(i in 1:nage)
    {
      for(j in 1:nage){
        M[i,j] = C[i,j]*f[i]/f[j]
      }
    }
    eig = eigen(M)
    beta = R0*gamma/max(Re(eig$values))  # reverse engineer beta from the R0 and gamma 
    beta = beta
  }else{
    beta = 0.05
  }
  results = list(beta,S_0,I_0,R_0,N,R0,npop)
  names(results) =c('beta','S_0','I_0','R_0','N','R0','npop')
  return(results)
}


SIRfunc_age=function(t, x, vparameters)
{
  nage = length(x)/3
  S    = as.matrix(x[1:nage])
  I    = as.matrix(x[(nage+1):(2*nage)])
  R    = as.matrix(x[(2*nage+1):(3*nage)])
  
  I[I<0] = 0
  with(as.list(vparameters),{
    N = S+I+R
    dS = -as.matrix(S*beta)*(as.matrix(C)%*%as.matrix(I/N))
    dI = -dS - gamma*as.matrix(I)
    dR = +gamma*as.matrix(I)
    out=c(dS,dI,dR)
    list(out)
  })
}


getSIR = function(COUNTRY,constraints,R0)
{
  npop = populationproportion[populationproportion$final_country==COUNTRY,29]
  f = populationproportion[populationproportion$final_country==COUNTRY,8:23]
  C = KAPPA.HOME.WORLD[[COUNTRY]]+KAPPA.WORK.WORLD[[COUNTRY]]+KAPPA.SCHOOL.WORLD[[COUNTRY]]+KAPPA.OTHERS.WORLD[[COUNTRY]]
  C_nosch = constraints[1]*KAPPA.HOME.WORLD[[COUNTRY]]+constraints[2]*KAPPA.WORK.WORLD[[COUNTRY]]+constraints[3]*KAPPA.SCHOOL.WORLD[[COUNTRY]]+constraints[4]*KAPPA.OTHERS.WORLD[[COUNTRY]]
  
  results = getbeta(npop = as.numeric(npop), f= as.numeric(f),C =C,R0=R0);results$beta
  # results_nosch = getbeta(npop = as.numeric(npop), f= as.numeric(f),C =C_nosch)
  # results_nosch$beta
  
  vparameters = list(gamma=1,beta=results$beta,C=C)
  vparameters_nosch = list(gamma=1,beta=results$beta,C=C_nosch)
  
  # S_0 = results$S_0
  # I_0 = results$I_0
  # R_0 = results$R_0
  inits = c(S=results$S_0,I=results$I_0,R=c(0.018,0.037,0.037,rep(0.098,7),rep(0.143,3),rep(0.211,2),0.187)*results$npop)#results$R_0)
  
  vt = seq(0,100,0.5)  # let's determine the values of S,I and R at times in vt
  sirage = as.data.frame(lsoda(inits, vt, SIRfunc_age, vparameters))
  sirage_nosch = as.data.frame(lsoda(inits, vt, SIRfunc_age, vparameters_nosch))
  output = list(results,sirage,sirage_nosch)
  names(output) = c('results','sirage','sirage_nosch')
  return(output)
}

getSIRmatrix = function(COUNTRY,constraints,R0)
{
  npop = populationproportion[populationproportion$final_country==COUNTRY,29]
  f = populationproportion[populationproportion$final_country==COUNTRY,8:23]
  C = KAPPA.HOME.WORLD[[COUNTRY]]+KAPPA.WORK.WORLD[[COUNTRY]]+KAPPA.SCHOOL.WORLD[[COUNTRY]]+KAPPA.OTHERS.WORLD[[COUNTRY]]
  # C_nosch = constraints[1]*KAPPA.HOME.WORLD[[COUNTRY]]+constraints[2]*KAPPA.WORK.WORLD[[COUNTRY]]+constraints[3]*KAPPA.SCHOOL.WORLD[[COUNTRY]]+constraints[4]*KAPPA.OTHERS.WORLD[[COUNTRY]]
  C_nosch = constraints[[1]]%*%KAPPA.HOME.WORLD[[COUNTRY]]%*%constraints[[1]] +
    constraints[[2]]%*%KAPPA.WORK.WORLD[[COUNTRY]]%*%constraints[[2]] +
    constraints[[3]]%*%KAPPA.SCHOOL.WORLD[[COUNTRY]]%*%constraints[[3]] +
    constraints[[4]]%*%KAPPA.OTHERS.WORLD[[COUNTRY]]%*%constraints[[4]] 
  
  results = getbeta(npop = as.numeric(npop), f= as.numeric(f),C =C,R0=R0);results$beta
  # results_nosch = getbeta(npop = as.numeric(npop), f= as.numeric(f),C =C_nosch)
  
  # results_nosch$beta
  
  vparameters = list(gamma=1,beta=results$beta,C=C)
  vparameters_nosch = list(gamma=1,beta=results$beta,C=C_nosch)
  
  # S_0 = results$S_0
  # I_0 = results$I_0
  # R_0 = results$R_0
  inits = c(S=results$S_0,I=results$I_0,R=results$R_0)
  
  vt = seq(0,100,0.5)  # let's determine the values of S,I and R at times in vt
  sirage = as.data.frame(lsoda(inits, vt, SIRfunc_age, vparameters))
  sirage_nosch = as.data.frame(lsoda(inits, vt, SIRfunc_age, vparameters_nosch))
  output = list(results,sirage,sirage_nosch)
  names(output) = c('results','sirage','sirage_nosch')
  return(output)
}


getPlotAttacksize = function(COUNTRYNAME,Nage,ATTACKSIZEage,ATTACKSIZEage_nosch,MAXY,YAXISTICKS,YAXISLABEL,results,INTERVENTION,LEFTSIDE = FALSE,BOTTOMSIDE=FALSE,LEGEND=FALSE,STRIP=FALSE,SCHOOL)
{
  if(SCHOOL==TRUE) COLOURWORKSCHOOL = 'steelblue'
  if(SCHOOL==FALSE) COLOURWORKSCHOOL = '#dd1c77'
  # grid.newpage()
  if(STRIP == TRUE) pushViewport(plotViewport(c(3.5,3.5,3,1),xscale=c(0,16),yscale=c(0,MAXY)))
  if(STRIP == FALSE & BOTTOMSIDE == FALSE) pushViewport(plotViewport(c(1.5,2.0,1,0.5),xscale=c(0,16),yscale=c(0,MAXY)))
  if(STRIP == FALSE & BOTTOMSIDE == TRUE) pushViewport(plotViewport(c(2,2.0,1,0.5),xscale=c(0,16),yscale=c(0,MAXY)))
  
  grid.rect()
  grid.xaxis(at=seq(0,16,2),label=seq(0,80,10),gp=gpar(fontsize=9))
  grid.yaxis(at=YAXISTICKS,label=YAXISLABEL,gp=gpar(fontsize=9))
  
  if(BOTTOMSIDE == TRUE){ grid.text('Age',y=unit(-2.1,'lines'),gp=gpar(fontsize=11)) }
  if(LEFTSIDE== TRUE){  grid.text('No. of individuals (mil)',x=unit(-2.0,'lines'),rot=90,gp=gpar(fontsize=11))}
  
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,Nage[AGE],Nage[AGE]),units = 'native')
                 ,gp = gpar(fill='grey40',col='white'))
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,ATTACKSIZEage[AGE],ATTACKSIZEage[AGE]),units = 'native')
                 ,gp = gpar(fill='darkorange',col='white'))
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,ATTACKSIZEage_nosch[AGE],ATTACKSIZEage_nosch[AGE]),units = 'native')
                 ,gp = gpar(fill=COLOURWORKSCHOOL,col='white'))
  #   
  #   if(STRIP == TRUE) 
  #   {
  #     grid.text(expression(paste(R[0]) ),unit(0.43,'npc'),unit(1,'npc')+unit(-0.8,'lines'),just = c('left'),gp=gpar(fontsize=12))
  #     grid.text(paste('=',results$R0) ,unit(0.48,'npc'),unit(1,'npc')+unit(-0.8,'lines'),just = c('left'),gp=gpar(fontsize=12))
  #     
  #   }
  
  if(LEGEND== TRUE){
    grid.rect(gp=gpar(col='black',fill=NA))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.45,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)+0.06,units = 'npc')
                 ,gp = gpar(fill='darkorange',col='darkorange'))
    grid.text(paste0('No intervention'),unit(0.48,'npc'),unit(0.835+0.06,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.45,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)-0.12,units = 'npc')
                 ,gp = gpar(fill='#dd1c77',col='#dd1c77'))
    grid.text(paste0('50% Workforce'),unit(0.48,'npc'),unit(0.83-0.06,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.45,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)-0.06,units = 'npc')
                 ,gp = gpar(fill='steelblue',col='steelblue'))
    grid.text(paste0('SC + Distancing'),unit(0.48,'npc'),unit(0.83-0.06,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.45,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85),units = 'npc')
                 ,gp = gpar(fill='steelblue',col='steelblue'))
    # grid.text(paste0(INTERVENTION),unit(0.46,'npc'),unit(0.865,'npc'),just = c('left'),gp=gpar(fontsize=7))
    grid.text(paste0('School Closure (SC)'),unit(0.48,'npc'),unit(0.835,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.45,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)+0.12,units = 'npc')
                 ,gp = gpar(fill='grey40',col='grey40'))
    #   grid.polygon(x=unit(c(0.85,0.881,0.881,0.85)-0.45,units = 'npc'),y=unit(c(0.85,0.85,0.88,0.88)+0.1,units = 'npc')
    #                ,gp = gpar(fill='darkorange',col='darkorange'))
    #   grid.polygon(x=unit(c(0.85,0.867,0.867,0.85)-0.45,units = 'npc'),y=unit(c(0.85,0.85,0.88,0.88)+0.1,units = 'npc')
    #                ,gp = gpar(fill='grey40',col='grey40'))
    grid.text(paste0('Total Population'),unit(0.48,'npc'),unit(0.835+0.12,'npc'),just = c('left'),gp=gpar(fontsize=7))
  }
  if(STRIP == TRUE) grid.text(paste0('Population size = ',signif(results$npop,2)/10E5,' mil'),unit(0.01,'npc'),unit(0.98,'npc'),just = c('left'),gp=gpar(fontsize=8))
  if(STRIP == TRUE) grid.text(paste0(COUNTRYNAME),unit(0.5,'npc'),unit(1,'npc')+unit(1,'lines'),just = c('center'),gp=gpar(fontsize=13,fontface='bold'))
  
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

getPlotAttacksize = function(COUNTRYNAME,Nage,ATTACKSIZEage,ATTACKSIZEage_nosch,MAXY,YAXISTICKS,YAXISLABEL,results,INTERVENTION,LEFTSIDE = FALSE,RIGHTSIDE = FALSE,BOTTOMSIDE=FALSE,LEGEND=FALSE,STRIP=FALSE,SCHOOL,DISTANCING=FALSE)
{
  
  
  if(SCHOOL==TRUE) COLOURWORKSCHOOL = 'steelblue'
  if(SCHOOL==TRUE&DISTANCING==TRUE) COLOURWORKSCHOOL = 'mediumseagreen'
  if(SCHOOL==FALSE) COLOURWORKSCHOOL = '#dd1c77'
  # grid.newpage()
  if(STRIP == TRUE) pushViewport(plotViewport(c(3.5,3.5,3,1),xscale=c(0,16),yscale=c(0,MAXY)))
  if(STRIP == FALSE & BOTTOMSIDE == FALSE) pushViewport(plotViewport(c(1.5,2.0,1,1.1),xscale=c(0,16),yscale=c(0,MAXY)))
  if(STRIP == FALSE & BOTTOMSIDE == TRUE) pushViewport(plotViewport(c(2,2.0,1,1.1),xscale=c(0,16),yscale=c(0,MAXY)))
  
  grid.rect()
  grid.xaxis(at=seq(0,16,2),label=seq(0,80,10),gp=gpar(fontsize=9))
  grid.yaxis(at=YAXISTICKS,label=YAXISLABEL,gp=gpar(fontsize=9))
  YAXISTICKS_right = seq(0,MAXY,length.out = 12)[c(1,3,5,7,9,11)]
  grid.yaxis(at=YAXISTICKS_right,label=F,gp=gpar(fontsize=7),main = FALSE)
  grid.text(c('0','20','40','60','80','100'),x=unit(1,'lines')+unit(1,'npc'),y=unit(YAXISTICKS_right,'native'),rot=270,gp=gpar(fontsize=7))
  
  if(BOTTOMSIDE == TRUE){ grid.text('Age',y=unit(-2.1,'lines'),gp=gpar(fontsize=11)) }
  if(LEFTSIDE== TRUE){  grid.text('No. of individuals (mil)',x=unit(-2.0,'lines'),rot=90,gp=gpar(fontsize=11))}
  if(RIGHTSIDE== TRUE){  grid.text('% Reduction',x=unit(1.35,'lines')+unit(1,'npc'),rot=270,gp=gpar(fontsize=10))}
  
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,Nage[AGE],Nage[AGE]),units = 'native')
                 ,gp = gpar(fill='grey40',col='white'))
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,ATTACKSIZEage[AGE],ATTACKSIZEage[AGE]),units = 'native')
                 ,gp = gpar(fill='darkorange',col='white'))
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,ATTACKSIZEage_nosch[AGE],ATTACKSIZEage_nosch[AGE]),units = 'native')
                 ,gp = gpar(fill=COLOURWORKSCHOOL,col='white'))
  
  reduction = ((ATTACKSIZEage - ATTACKSIZEage_nosch)/ATTACKSIZEage*100)/110*MAXY
  grid.lines(x =unit(c(1:16)-0.5,'native'),y = unit(reduction,'native'),gp = gpar(lwd=2,col='black'))
  #   
  #   if(STRIP == TRUE) 
  #   {
  #     grid.text(expression(paste(R[0]) ),unit(0.43,'npc'),unit(1,'npc')+unit(-0.8,'lines'),just = c('left'),gp=gpar(fontsize=12))
  #     grid.text(paste('=',results$R0) ,unit(0.48,'npc'),unit(1,'npc')+unit(-0.8,'lines'),just = c('left'),gp=gpar(fontsize=12))
  #     
  #   }
  
  if(LEGEND== TRUE){
    grid.rect(gp=gpar(col='black',fill=NA))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)+0.06,units = 'npc')
                 ,gp = gpar(fill='darkorange',col='darkorange'))
    grid.text(paste0('No intervention'),unit(0.47,'npc'),unit(0.835+0.06,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)-0.12,units = 'npc')
                 ,gp = gpar(fill='#dd1c77',col='#dd1c77'))
    grid.text(paste0('50% Workforce'),unit(0.47,'npc'),unit(0.83-0.12,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)-0.06,units = 'npc')
                 ,gp = gpar(fill='mediumseagreen',col='mediumseagreen'))
    grid.text(paste0('SC + Distancing'),unit(0.47,'npc'),unit(0.83-0.06,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85),units = 'npc')
                 ,gp = gpar(fill='steelblue',col='steelblue'))
    grid.text(paste0('School Closure(SC)'),unit(0.47,'npc'),unit(0.835,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)+0.12,units = 'npc')
                 ,gp = gpar(fill='grey40',col='grey40'))

    grid.text(paste0('Total Population'),unit(0.47,'npc'),unit(0.835+0.12,'npc'),just = c('left'),gp=gpar(fontsize=7))
  }
  if(STRIP == TRUE) grid.text(paste0('Population size = ',signif(results$npop,2)/10E5,' mil'),unit(0.01,'npc'),unit(0.98,'npc'),just = c('left'),gp=gpar(fontsize=8))
  if(STRIP == TRUE) grid.text(paste0(COUNTRYNAME),unit(0.5,'npc'),unit(1,'npc')+unit(1,'lines'),just = c('center'),gp=gpar(fontsize=13,fontface='bold'))
  
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

getPlotAttacksize = function(COUNTRYNAME,Nage,ATTACKSIZEage,ATTACKSIZEage_nosch,MAXY,YAXISTICKS,YAXISLABEL,results,INTERVENTION,LEFTSIDE = FALSE,RIGHTSIDE = FALSE,BOTTOMSIDE=FALSE,LEGEND=FALSE,STRIP=FALSE,SCHOOL,DISTANCING=FALSE)
{
  
  
  # if(SCHOOL==TRUE) COLOURWORKSCHOOL = 'steelblue'
  if(SCHOOL==TRUE&DISTANCING==TRUE) COLOURWORKSCHOOL = 'steelblue'
  # if(SCHOOL==TRUE&DISTANCING==TRUE) COLOURWORKSCHOOL = 'mediumseagreen'
  if(SCHOOL==FALSE) COLOURWORKSCHOOL = '#dd1c77'
  # grid.newpage()
  if(STRIP == TRUE) pushViewport(plotViewport(c(3.5,3.5,3,1),xscale=c(0,16),yscale=c(0,MAXY)))
  if(STRIP == FALSE & BOTTOMSIDE == FALSE) pushViewport(plotViewport(c(1.5,2.0,1,1.1),xscale=c(0,16),yscale=c(0,MAXY)))
  if(STRIP == FALSE & BOTTOMSIDE == TRUE) pushViewport(plotViewport(c(2,2.0,1,1.1),xscale=c(0,16),yscale=c(0,MAXY)))
  
  grid.rect()
  grid.xaxis(at=seq(0,16,2),label=seq(0,80,10),gp=gpar(fontsize=9))
  grid.yaxis(at=YAXISTICKS,label=YAXISLABEL,gp=gpar(fontsize=9))
  YAXISTICKS_right = seq(0,MAXY,length.out = 12)[c(1,3,5,7,9,11)]
  grid.yaxis(at=YAXISTICKS_right,label=F,gp=gpar(fontsize=7),main = FALSE)
  grid.text(c('0','20','40','60','80','100'),x=unit(1,'lines')+unit(1,'npc'),y=unit(YAXISTICKS_right,'native'),rot=270,gp=gpar(fontsize=7))
  
  if(BOTTOMSIDE == TRUE){ grid.text('Age',y=unit(-2.1,'lines'),gp=gpar(fontsize=11)) }
  if(LEFTSIDE== TRUE){  grid.text('No. of individuals (mil)',x=unit(-2.0,'lines'),rot=90,gp=gpar(fontsize=11))}
  if(RIGHTSIDE== TRUE){  grid.text('% Reduction',x=unit(1.35,'lines')+unit(1,'npc'),rot=270,gp=gpar(fontsize=10))}
  
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,Nage[AGE],Nage[AGE]),units = 'native')
                 ,gp = gpar(fill='grey40',col='white'))
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,ATTACKSIZEage[AGE],ATTACKSIZEage[AGE]),units = 'native')
                 ,gp = gpar(fill='darkorange',col='white'))
  for(AGE in 1:16) 
    grid.polygon(x=unit(c(AGE-1,AGE,AGE,AGE-1),units = 'native'),y=unit(c(0,0,ATTACKSIZEage_nosch[AGE],ATTACKSIZEage_nosch[AGE]),units = 'native')
                 ,gp = gpar(fill=COLOURWORKSCHOOL,col='white'))
  
  reduction = ((ATTACKSIZEage - ATTACKSIZEage_nosch)/ATTACKSIZEage*100)/110*MAXY
  reduction[reduction<0] = 0
  grid.lines(x =unit(c(1:16)-0.5,'native'),y = unit(reduction,'native'),gp = gpar(lwd=2,col='black'))
  #   
  #   if(STRIP == TRUE) 
  #   {
  #     grid.text(expression(paste(R[0]) ),unit(0.43,'npc'),unit(1,'npc')+unit(-0.8,'lines'),just = c('left'),gp=gpar(fontsize=12))
  #     grid.text(paste('=',results$R0) ,unit(0.48,'npc'),unit(1,'npc')+unit(-0.8,'lines'),just = c('left'),gp=gpar(fontsize=12))
  #     
  #   }
  
  if(LEGEND== TRUE){
    grid.rect(gp=gpar(col='black',fill=NA))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)+0.06,units = 'npc')
                 ,gp = gpar(fill='darkorange',col='darkorange'))
    grid.text(paste0('No intervention'),unit(0.47,'npc'),unit(0.835+0.06,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)-0.06,units = 'npc')
                 ,gp = gpar(fill='#dd1c77',col='#dd1c77'))
    grid.text(paste0('50% Workforce'),unit(0.47,'npc'),unit(0.83-0.06,'npc'),just = c('left'),gp=gpar(fontsize=7))
    grid.text(paste0('SC: School Closure'),unit(0.39,'npc'),unit(0.83-0.12,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    # grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)-0.06,units = 'npc')
    #              ,gp = gpar(fill='mediumseagreen',col='mediumseagreen'))
    # grid.text(paste0('SC + Distancing'),unit(0.47,'npc'),unit(0.83-0.06,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85),units = 'npc')
                 ,gp = gpar(fill='steelblue',col='steelblue'))
    grid.text(paste0('SC + Distancing'),unit(0.47,'npc'),unit(0.835,'npc'),just = c('left'),gp=gpar(fontsize=7))
    
    grid.polygon(x=unit(c(0.85,0.9,0.9,0.85)-0.46,units = 'npc'),y=unit(c(0.82,0.82,0.85,0.85)+0.12,units = 'npc')
                 ,gp = gpar(fill='grey40',col='grey40'))
    
    grid.text(paste0('Total Population'),unit(0.47,'npc'),unit(0.835+0.12,'npc'),just = c('left'),gp=gpar(fontsize=7))
  }
  if(STRIP == TRUE) grid.text(paste0('Population size = ',signif(results$npop,2)/10E5,' mil'),unit(0.01,'npc'),unit(0.98,'npc'),just = c('left'),gp=gpar(fontsize=8))
  if(STRIP == TRUE) grid.text(paste0(COUNTRYNAME),unit(0.5,'npc'),unit(1,'npc')+unit(1,'lines'),just = c('center'),gp=gpar(fontsize=13,fontface='bold'))
  
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}



getSIRplot = function(TIME=100,MAXY,YLABEL,YVALUES,finalattacksize,AGERANGE)
{
  pushViewport(plotViewport(c(2,2,1,1),xscale=c(0,TIME),yscale=c(0,MAXY)))
  grid.rect()
  # grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6))
  grid.yaxis(at=seq(0,MAXY,by=MAXY/5),label=T,gp=gpar(fontsize=6))
  
  grid.text('Time',y=unit(-2.1,'lines'),gp=gpar(fontsize=9))
  # grid.text('Age group of contact',x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7))
  grid.text(YLABEL,x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=9))
  
  grid.lines(x=unit(c(sirage$time),units = 'native'),y=unit(c(YVALUES),units = 'native'),gp=gpar(lwd=2,col='black'))
  grid.text(paste0('Final attack size = ',finalattacksize),unit(0.01,'npc'),unit(1,'npc')+unit(-0.8,'lines'),just = c('left'),gp=gpar(fontsize=6.5))
  grid.text(paste0('Ages ',(AGERANGE-1)*5,' to ',(AGERANGE)*5),unit(0.5,'npc'),unit(1,'npc')+unit(-0.8,'lines'),just = c('center'),gp=gpar(fontsize=8))
  
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}



getPlot_hhsize = function(nrow,ncol,cols,SIZE,a,INDEX){
  pushViewport(plotViewport(c(2.3,2.5,2,1),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  if(nrow>50){
    grid.xaxis()
    grid.yaxis()
  }else {
    grid.xaxis(at=(0:nrow)*a,label = F,gp=gpar(fontsize=4))
    grid.yaxis(at=(0:ncol)*a,label = F,gp=gpar(fontsize=4))
    grid.xaxis(at=(0:(nrow/2)*10),gp=gpar(fontsize=7))
    grid.yaxis(at=(0:(ncol/2)*10),gp=gpar(fontsize=7))
  }
  
  grid.text(paste('Household size',SIZE),
            0.5,unit(1.05,'npc')+unit(0,'lines'), gp=gpar(fontsize=9))
  grid.text('Age of Individual',y=unit(-2.5,'lines'),gp=gpar(fontsize=8))
  grid.text('Age of Household Contact',x=unit(-3,'lines'),rot=90,gp=gpar(fontsize=8))
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  grid.text(paste0('(',INDEX,')'),unit(0.05,'npc'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(col='black',fill=NA)) 
}



getCorrelationPlot = function(MAX,INDEX,EMPIRICAL,MODELLED,XAXISNAME = 'Empirical POLYMOD / DHS data',YAXISNAME = 'Modelled HAM',FONTSIZEPLUS = 0){
  # grid.newpage()
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(-0.01,(MAX+0.1)),yscale=c(-0.01,(MAX+0.1))))
  grid.rect()
  grid.xaxis(gp=gpar(fontsize=6))
  grid.yaxis(gp=gpar(fontsize=6))
  
  grid.text(XAXISNAME,y=unit(-2.1,'lines'),gp=gpar(fontsize=7+FONTSIZEPLUS))
  grid.text(YAXISNAME,x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7+FONTSIZEPLUS))
  
  matrixagecolour = rep(seq(1,16,1),times = 16)
  scaleagecolour = viridis(max(matrixagecolour))
  # scaleagecolour = 'grey40'
  grid.points(x=c(as.vector(EMPIRICAL)),y=c(as.vector(MODELLED)),size = unit(.5, "char"), pch=20,default.units = "native",gp=gpar(col=scaleagecolour,fill=scaleagecolour,alpha = .8))
  grid.lines(x=unit(c(0,MAX+.09),units = 'native'),y=unit(c(0,MAX+.09),units = 'native'),gp=gpar(lwd=2,lty='dashed',col='black'))
  grid.text(paste0('r = ',round(cor(as.vector(EMPIRICAL),as.vector(MODELLED)),2)),unit(0.5,'npc'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=8+FONTSIZEPLUS))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5+FONTSIZEPLUS))
  
  ypos = seq(0.05,0.7,length.out = 16)
  allages = paste0(seq(0,75,5),'-',seq(4,79,5))
  grid.points(x = unit(rep(1,ncol(MODELLED)),'npc')+unit(0.2,'lines'),y = unit(ypos[1:ncol(MODELLED)],'npc'),size = unit(.5, "char"), pch=20,
              gp=gpar(col=scaleagecolour[1:ncol(MODELLED)],fill=scaleagecolour[1:ncol(MODELLED)],alpha = 1))
  grid.text(allages[1:ncol(MODELLED)],x = unit(rep(1,ncol(MODELLED)),'npc')+unit(0.75,'lines'),y = unit(ypos[1:ncol(MODELLED)],'npc'),just = 'left',gp=gpar(fontsize = 5+FONTSIZEPLUS))
  
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}

plotBarPlotbyAge = function(DATA,YLAB = 'Mean number of HH members',LEGEND=FALSE, INDEX)
{
  # grid.newpage()
  pushViewport(plotViewport(c(2.5,1.5,0.5,0.5),xscale=c(0,16),yscale=c(0,max(DATA$q3)+2)))
  grid.rect()
  grid.yaxis(gp=gpar(fontsize = 7))
  grid.xaxis(at = seq(0,16,2),label = seq(0,80,10), gp=gpar(fontsize = 7))
  grid.text(YLAB, x=unit(-2.50, "lines"), rot=90,gp=gpar(fontsize=7))
  grid.text("Age of individual", y=unit(-2.50, "lines"), gp=gpar(fontsize=7))
  
  for(i in 1:16) 
  {
    value = DATA$mean[i]
    value_high =  DATA$q3[i]
    value_low =  DATA$q1[i]
    grid.polygon(x = c((i-1),i,i,(i-1)),y= c(0,0,value,value),default.units = 'native',gp=gpar(fill='grey40',col='white',lwd=1))
    grid.lines(x=i-0.5,y = c(value_low,value),default.units = 'native',gp=gpar(col='black',lwd=2))
    grid.lines(x=i-0.5,y = c(value_high,value),default.units = 'native',gp=gpar(col='black',lwd=2))
    grid.points(x=i-0.5, y =  DATA$median[i],default.units = 'native',pch=16,gp=gpar(col='black',cex=0.75))
    
  }
  if(LEGEND)
  {
    grid.text("Median & IQR", x = unit(0.7,'npc'), y=unit(0.95, "npc"),just = 'left', gp=gpar(fontsize=7))
    grid.lines(x = unit(c(0.645,0.685),'npc'), y=unit(0.95, "npc"),gp=gpar(col='black',lwd=1))
    grid.points(x = unit(0.665,'npc'), y=unit(0.95, "npc"),pch=16,gp=gpar(col='black',cex=0.5))
  }
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5))
  grid.rect(gp=gpar(fill=NA,lwd=1))
  popViewport()
}