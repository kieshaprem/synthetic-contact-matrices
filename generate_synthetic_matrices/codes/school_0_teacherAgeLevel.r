# OECD (2020), Teachers by age (indicator). doi: 10.1787/93af1f9d-en 
teachers_byageandlevel = read.csv('input/school/DP_LIVE_18052020104832036.csv',stringsAsFactors = FALSE)

iso = unique(teachers_byageandlevel$LOCATION)

schoollevels = c("PRY",'SRY','TRY')
agelevels = c('LT30','30_39','40_49','50_OVER')

teacheragelevel = list() 
for(j in 1:3)
{
  te = data.frame(iso=iso,array(NA,c(length(iso),4)))
  colnames(te)[-1] = agelevels
  for(i in 1:nrow(te))
  {
    ISO = (te$iso[i])
    alllevels = paste0(schoollevels[j],'_',agelevels)
    for(k in 1:length(alllevels))
    {
      index = which((teachers_byageandlevel$LOCATION %in% ISO) & (teachers_byageandlevel$SUBJECT%in% alllevels[k]) & (!is.na(teachers_byageandlevel$Value)))
      if(length(index)>0)
      {
        te[i,k+1] = teachers_byageandlevel$Value[index[which.max(teachers_byageandlevel$TIME[index])]]/100
      }
    }
  }
  teacheragelevel[[j]] = te
}



proportion_teacheragelevel = list()#colMeans(teacheragelevel[,-1],na.rm = TRUE)
proportion_teacheragelevel$preprimary = colMeans(teacheragelevel[[1]][,2:5],na.rm = TRUE)
proportion_teacheragelevel$primary = colMeans(teacheragelevel[[1]][,2:5],na.rm = TRUE)
proportion_teacheragelevel$secondary = colMeans(teacheragelevel[[2]][,2:5],na.rm = TRUE)
proportion_teacheragelevel$tertiary = colMeans(teacheragelevel[[3]][,2:5],na.rm = TRUE)
save(proportion_teacheragelevel,file = 'input/school/proportion_teacheragelevel.rdata')
