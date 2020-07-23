# load modelling output for the physical distancing interventions for COVID-19 pandemic using both the empirical and synthetic contact matrices 
# (codes are hosted in a different repository)

load('output/data_effectiveness.rdata')

library(ggsci)
COLS = pal_npg(c("nrc"))(10)
COLS = COLS[c(1,5,3,4,2,6,7,8,9,10)]
iso= c("CHN", "FRA", "HKG", "KEN", "PER", "RUS", "ZAF", "UGA", "VNM", "ZWE")
countries = list(name = unique(data_effectiveness$country), iso = iso)
reduction = list(iso = countries$iso, name = countries$name)
reduction$fullname = NA 
for(i in 1:10) reduction$fullname[i] = as.character(poptotal$countryname[poptotal$iso3c %in% reduction$iso[i]])
reduction$fullname[1] = 'China'
reduction$fullname[3] = "Hong Kong SAR, China"
reduction$distance20 = list()
reduction$distance50 = list()
reduction$shielding = list()

for(co in 1:length(reduction$iso))
{
  INDEX1 = which(data_effectiveness$scen %in% 'social distancing20'& data_effectiveness$country %in% reduction$name[co] & data_effectiveness$variable %in% 'effectiveness' & data_effectiveness$compartment %in% 'cases')
  reduction$distance20[[reduction$iso[co]]] = data.frame(data_effectiveness[INDEX1,c('contact_matrix','low95','low50','median','high50','high95')])
  INDEX2 = which(data_effectiveness$scen %in% 'social distancing50'& data_effectiveness$country %in% reduction$name[co] & data_effectiveness$variable %in% 'effectiveness' & data_effectiveness$compartment %in% 'cases')
  reduction$distance50[[reduction$iso[co]]] = data.frame(data_effectiveness[INDEX2,c('contact_matrix','low95','low50','median','high50','high95')])
  INDEX3 = which(data_effectiveness$scen %in% 'shielding'& data_effectiveness$country %in% reduction$name[co] & data_effectiveness$variable %in% 'effectiveness' & data_effectiveness$compartment %in% 'cases')
  reduction$shielding[[reduction$iso[co]]] = data.frame(data_effectiveness[INDEX3,c('contact_matrix','low95','low50','median','high50','high95')])
  rm(INDEX1,INDEX2,INDEX3)
}


if(SAVEPLOT) png('plots/impcat_relative_reduction_cases.png',height=17,width=15,units='cm',res=1000,pointsize=10)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(2.5,0,0,0)))
  pushViewport(viewport(layout=grid.layout(nrow=5,ncol=2,width=c(rep(1,2)),height=c(rep(1,5)))))
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  plotReduction(ISO = countries$iso[1],INDEX = letters[1])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  plotReduction(ISO = countries$iso[2],INDEX = letters[2])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=3))
  plotReduction(ISO = countries$iso[3],INDEX = letters[3])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=4))
  plotReduction(ISO = countries$iso[4],INDEX = letters[4])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=5))
  plotReduction(ISO = countries$iso[5],INDEX = letters[5])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
  plotReduction(ISO = countries$iso[6],INDEX = letters[6])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
  plotReduction(ISO = countries$iso[7],INDEX = letters[7])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=3))
  plotReduction(ISO = countries$iso[8],INDEX = letters[8])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=4))
  plotReduction(ISO = countries$iso[9],INDEX = letters[9])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=5))
  plotReduction(ISO = countries$iso[10],INDEX = letters[10])
  popViewport()
  
  popViewport()
  matr_names = c("Empirical\n(study year)","Empirical\n(adjusted)","Synthetic 2017","Synthetic 2020", "Synthetic 2020\n(rural)",   
                 "Synthetic 2020\n(urban)")
  start = seq(0.025,0.975,length.out = length(matr_names)+1)
  xpos = c(0,0.01)
  ypos = seq(0.0,0.04,length.out = 5)
  
  for(m in 1:6)
  {
    grid.lines(x = unit(mean(c(xpos[1],xpos[2]))+start[m],'npc'), y = unit(c(ypos[1],ypos[5]), 'npc')-unit(2,'lines'), gp = gpar(col = COLS[m]) )
    grid.polygon(x = unit(c(xpos[1],xpos[2],xpos[2],xpos[1])+start[m],'npc'), y = unit(c(ypos[2],ypos[2],ypos[4],ypos[4]), 'npc')-unit(2,'lines'), gp = gpar(col = NA, fill = COLS[m]))
    grid.lines(x = unit((c(xpos[1],xpos[2]))+start[m],'npc'), y = unit((c(ypos[3],ypos[3])), 'npc')-unit(2,'lines'), gp = gpar(col = 'white') )
    grid.text(matr_names[m],x = unit(mean(c(xpos[1],xpos[2]))+0.015+start[m],'npc'), y = unit(c(ypos[3]), 'npc')-unit(3,'lines'),just = 'left',gp = gpar(fontsize = 7,fontface = 'bold'))
  }
  
  popViewport()
  
  
}
if(SAVEPLOT) dev.off()
