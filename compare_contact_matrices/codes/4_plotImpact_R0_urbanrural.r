# Calculate R0
# contact_matrix[i,j] is the number of age-j individuals an age-i individual contacts per day, and can potentially be infected by.
# beta is the transmission rate
# susceptibility[i] is the probability of infection on effective contact, for a person of age i (can set to 1 for no variation)
# infectiousness[i] is the relative infectiousness of an age-i individual (can set to 1 for no variation)
# duration_of_infectiousness[i] is the length of time an age-i individual remains infectious
# susceptibility, infectiousness, and duration_of_infection can either be scalars or vectors.
calc_R0 = function(contact_matrix, beta, susceptibility, infectiousness, duration_of_infectiousness)
{
  ngm = beta * susceptibility * t(t(contact_matrix) * (infectiousness * duration_of_infectiousness))
  abs(eigen(ngm)$values[1])
}

countries = names(contacts2020_rural)

r0_urbanrural = data.frame(iso = countries, 
                           contacts_0to9_urban = NA, contacts_0to9_rural = NA, 
                           contacts_60to69_urban = NA, contacts_60to69_rural = NA,
                           r0_urban = NA, r0_rural = NA,
                           r0_urban_age = NA, r0_rural_age = NA)
for(i in 1:nrow(r0_urbanrural))
{
  co = r0_urbanrural$iso[i]
  
  r0_urbanrural$contacts_0to9_urban[i] = mean(rowSums(contacts2020_urban[[co]]$all)[1:2])
  r0_urbanrural$contacts_60to69_urban[i] = mean(rowSums(contacts2020_urban[[co]]$all)[13:14])
  r0_urbanrural$contacts_0to9_rural[i] = mean(rowSums(contacts2020_rural[[co]]$all)[1:2])
  r0_urbanrural$contacts_60to69_rural[i] = mean(rowSums(contacts2020_rural[[co]]$all)[13:14])
  
  r0_urbanrural$r0_urban[i] = calc_R0(contact_matrix = contacts2020_urban[[co]]$all, 
                                      beta = 0.04,
                                      susceptibility =  rep(1,16),
                                      infectiousness =  rep(1,16), 
                                      duration_of_infectiousness = 5) 
  r0_urbanrural$r0_urban_age[i] = calc_R0(contact_matrix = contacts2020_urban[[co]]$all, 
                                          beta = 0.04,
                                          susceptibility =  c(rep(0.5,4),rep(1,12)),
                                          infectiousness =  c(rep(0.5,4),rep(1,12)),
                                          duration_of_infectiousness = 5) 
  r0_urbanrural$r0_rural[i] = calc_R0(contact_matrix = contacts2020_rural[[co]]$all, 
                                      beta = 0.04,
                                      susceptibility =  rep(1,16),
                                      infectiousness =  rep(1,16), 
                                      duration_of_infectiousness = 5) 
  r0_urbanrural$r0_rural_age[i] = calc_R0(contact_matrix = contacts2020_rural[[co]]$all, 
                                          beta = 0.04,
                                          susceptibility =  c(rep(0.5,4),rep(1,12)),
                                          infectiousness =  c(rep(0.5,4),rep(1,12)),
                                          duration_of_infectiousness = 5) 
  
}

load('input/workpopage.rdata')
work = workpopage$total
r0_urbanrural$incomegroup = work$incomegroup[match(r0_urbanrural$iso,work$iso3c)]
r0_urbanrural$incomegroup_index = 1*(r0_urbanrural$incomegroup %in% "Low income")+
  2*(r0_urbanrural$incomegroup %in% "Lower-middle income")+
  3*(r0_urbanrural$incomegroup %in% "Upper-middle income")+ 
  4*(r0_urbanrural$incomegroup %in% "High income")

r0_urbanrural$subregion = work$subregion[match(r0_urbanrural$iso,work$iso3c)]
rm(work,workpopage)

if(SAVEPLOT) png(paste0('plots/impact_r0_urbanrural.png'),height=17,width=17,units='cm',res=700,pointsize=10)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(1,1.5,0,0)))
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2,width=c(1,1,1),height=c(1,1,1))))
  pushViewport(viewport(layout.pos.row = 1,layout.pos.col=1))
  plotCorrelation_urbanrural(RURAL = r0_urbanrural$contacts_0to9_rural,
                             URBAN = r0_urbanrural$contacts_0to9_urban,
                             INDEX = 'A',
                             XAXISNAME = 'Rural mean no. of contacts',
                             YAXISNAME = 'Urban mean no. of contacts',
                             SCALE = 1,YMAX = 26,YMIN = 5,
                             TITLE = 'Mean no. of contacts among 0-9 year olds',
                             GROUP = r0_urbanrural$incomegroup_index)
  popViewport()
  pushViewport(viewport(layout.pos.row = 1,layout.pos.col=2))
  plotCorrelation_urbanrural(RURAL = r0_urbanrural$contacts_60to69_rural,
                             URBAN = r0_urbanrural$contacts_60to69_urban,
                             INDEX = 'B',
                             XAXISNAME = 'Rural mean no. of contacts',
                             YAXISNAME = 'Urban mean no. of contacts',
                             SCALE = 1,YMAX = 20,YMIN = 0,
                             TITLE = 'Mean no. of contacts among 60-69 year olds',
                             GROUP = r0_urbanrural$incomegroup_index)
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 2,layout.pos.col=1))
  plotCorrelation_urbanrural(RURAL = r0_urbanrural$r0_rural,
                             URBAN = r0_urbanrural$r0_urban,
                             INDEX = 'C',
                             XAXISNAME = expression(paste('Rural ',R[0])),
                             YAXISNAME = expression(paste('Urban ',R[0])),
                             SCALE = 1,YMAX = 4.5,YMIN = 1,
                             TITLE = 'No age-dependent\nsusceptibilty and infectiousness',
                             GROUP = r0_urbanrural$incomegroup_index)
  popViewport()
  pushViewport(viewport(layout.pos.row = 2,layout.pos.col=2))
  plotCorrelation_urbanrural(RURAL = r0_urbanrural$r0_rural_age,
                             URBAN = r0_urbanrural$r0_urban_age,
                             INDEX = 'D', 
                             XAXISNAME = expression(paste('Rural ',R[0])),
                             YAXISNAME = expression(paste('Urban ',R[0])),
                             SCALE = 1,YMAX = 4.5,YMIN = 1,
                             TITLE = 'Age-dependent\nsusceptibilty and infectiousness',
                             GROUP = r0_urbanrural$incomegroup_index)
  popViewport()
  
  
  popViewport()
  popViewport()
  
}
if(SAVEPLOT) dev.off()


g = 4
cor.test(r0_urbanrural$contacts_0to9_rural[r0_urbanrural$incomegroup_index %in% g],
         r0_urbanrural$contacts_0to9_urban[r0_urbanrural$incomegroup_index %in% g])
cor.test(r0_urbanrural$r0_rural_age[r0_urbanrural$incomegroup_index %in% g],
         r0_urbanrural$r0_urban_age[r0_urbanrural$incomegroup_index %in% g])
cor.test(r0_urbanrural$r0_rural[r0_urbanrural$incomegroup_index %in% g],
         r0_urbanrural$r0_urban[r0_urbanrural$incomegroup_index %in% g])


