# load modelling output for the physical distancing interventions for COVID-19 pandemic using both the empirical and synthetic contact matrices 
# (codes are hosted in a different repository)

load('output/data_byage.rdata')

library(ggsci)
COLS = pal_npg(c("nrc"))(10)
COLS = COLS[c(1,5,3,4,2,6,7,8,9,10)]
iso= c("CHN", "FRA", "HKG", "KEN", "PER", "RUS", "ZAF", "UGA", "VNM", "ZWE")
countries = list(name = unique(data_byage$country), iso = iso)
rate = list(iso = countries$iso, name = countries$name)
rate$fullname = NA 
for(i in 1:10) rate$fullname[i] = as.character(poptotal$countryname[poptotal$iso3c %in% rate$iso[i]])
rate$fullname[1] = 'China'
rate$fullname[3] = "Hong Kong SAR, China"
rate$unmitigated = list()

data_byage$age = 0*(data_byage$group %in% "0-4") + 
  5*(data_byage$group %in% "5-9") +
  10*(data_byage$group %in% "10-14") +
  15*(data_byage$group %in% "15-19") +
  20*(data_byage$group %in% "20-24") +
  25*(data_byage$group %in% "25-29") +
  30*(data_byage$group %in% "30-34") +
  35*(data_byage$group %in% "35-39") +
  40*(data_byage$group %in% "40-44") +
  45*(data_byage$group %in% "45-49") +
  50*(data_byage$group %in% "50-54") +
  55*(data_byage$group %in% "55-59") +
  60*(data_byage$group %in% "60-64") +
  65*(data_byage$group %in% "65-69") +
  70*(data_byage$group %in% "70-74") +
  75*(data_byage$group %in% "75+") 

data_byage$cols =
  1*(data_byage$contact_matrix %in% "contact_matrix_empirical") +
  2*(data_byage$contact_matrix %in% "contact_matrix_empirical_adjusted") +
  3*(data_byage$contact_matrix %in% "contact_matrix_synthetic_old") +
  4*(data_byage$contact_matrix %in% "contact_matrix_synthetic_new_national") +
  5*(data_byage$contact_matrix %in% "contact_matrix_synthetic_new_rural") +
  6*(data_byage$contact_matrix %in% "contact_matrix_synthetic_new_urban") 

for(co in 1:length(rate$iso))
{
  INDEX1 = which(data_byage$scen %in% 'unmitigated'& data_byage$country %in% rate$name[co] & data_byage$compartment %in% 'S')
  rate$unmitigated[[rate$iso[co]]] = data.frame(iso = rate$iso[co], data_byage[INDEX1,c('cols','age','low95','low50','median','high50','high95')])
  rm(INDEX1)
}

if(SAVEPLOT) png('plots/impact_infection_attack_rate.png',height=17,width=20,units='cm',res=1000,pointsize=10)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(2.5,0,0,0)))
  pushViewport(viewport(layout=grid.layout(nrow=5,ncol=2,width=c(rep(1,2)),height=c(rep(1,5)))))
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  plotAttackRate(ISO = countries$iso[1],INDEX = letters[1])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  plotAttackRate(ISO = countries$iso[2],INDEX = letters[2])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=3))
  plotAttackRate(ISO = countries$iso[3],INDEX = letters[3])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=4))
  plotAttackRate(ISO = countries$iso[4],INDEX = letters[4])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=5))
  plotAttackRate(ISO = countries$iso[5],INDEX = letters[5])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
  plotAttackRate(ISO = countries$iso[6],INDEX = letters[6])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
  plotAttackRate(ISO = countries$iso[7],INDEX = letters[7])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=3))
  plotAttackRate(ISO = countries$iso[8],INDEX = letters[8])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=4))
  plotAttackRate(ISO = countries$iso[9],INDEX = letters[9])
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=5))
  plotAttackRate(ISO = countries$iso[10],INDEX = letters[10])
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

