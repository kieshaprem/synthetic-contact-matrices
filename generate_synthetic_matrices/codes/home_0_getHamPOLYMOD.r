
######################################################
# FUNCTIONS
######################################################


getHAMpolymod = function(COUNTRYCODE,AGEMAX=16)
{
  hh_age_country = hh_age_g5[part.all$country %in% COUNTRYCODE,]
  sizeHH  =  apply(hh_age_country,1,function(x) sum(!is.na(x)))
  hh_age_country[is.na(hh_age_country)] = 0
  hh_age_country[hh_age_country>AGEMAX] = AGEMAX
  
  n_country = sum(part.all$country %in% COUNTRYCODE)
  H=matrix(0,n_country,AGEMAX)
  for(i in 1:n_country)
  {
    for(a in 1:AGEMAX)
    {
      H[i,a] = sum(hh_age_country[i,]==a)
    }
  }
  rows = rep( 1:nrow(H) , sizeHH )
  Hfull = H[rows,]
  
  ageallHH = as.vector(t(hh_age_country))
  ageallHH = ageallHH[ageallHH>0]
  
  HHcolmeans = array(NA,c(AGEMAX,AGEMAX))
  for(i in 1:AGEMAX){
    HHcolmeans[i,]=colMeans(Hfull[ageallHH==i,])
  }
  image(HHcolmeans)
  return(HHcolmeans)
}

###################################################

library(countrycode)
READINDATA_POLYMOD = TRUE
CLEANHOUSEHOLD = TRUE
GETHAM_POLYMOD = TRUE
HAM_POLYMOD = list()
POLYMODcountries = c('belgium', 'germany','finland','united kingdom','italy','luxembourg','netherlands','poland')
POLYMODcountries_iso= countrycode(POLYMODcountries, origin = 'country.name', destination = 'iso3c')
if(READINDATA_POLYMOD)
{
  part.all =  read.table("input/polymod/participants_final.txt", header = T,sep="\t")
  contacts.all = read.table("input/polymod/contacts_final.txt", sep="\t",header = T)
  contacts.all = contacts.all[!is.na(contacts.all$cnt_age_mean)&!is.na(contacts.all$cnt_work),]
  part.all = part.all[is.element(part.all$global_id,contacts.all$global_id),]
  data.all = read.csv(file ="input/polymod/data.all(ALL countries).csv")
  rm(contacts.all)
}


if(CLEANHOUSEHOLD)
{
  n = nrow(part.all);n
  age_part = (part.all$participant_age%/%1)+1
  age_part[is.na(age_part)] = mean(age_part,na.rm =T)
  age_partA5 = age_part%/%5+1
  
  # Household 
  hh_agex=data.matrix(part.all[,18:37])+1
  head(hh_agex)
  sizereal =  apply(hh_agex,1,function(x) sum(!is.na(x)))
  size=part.all$hh_size
  sizediff = (size - sizereal)
  #   table(sizediff)
  #   problemHHage = list()
  #   problemHHage$sizedifferene1 = array(NA,sum(sizediff==1))
  #   problemHHage$sizedifferene2 = array(NA,sum(sizediff==2))
  #   problemHHage$sizedifferene3 = array(NA,sum(sizediff==3))
  #   problemHHage$sizedifferene4 = array(NA,sum(sizediff==4))
  #   problemHHage$sizedifferene5 = array(NA,sum(sizediff==5))
  #   problemHHage$sizedifferene6 = array(NA,sum(sizediff==6))
  #   problemHHage$sizedifferene7 = array(NA,sum(sizediff==7))
  #   problemHHage$sizedifferene8 = array(NA,sum(sizediff==8))
  #   
  #   for(j in 1:8)  
  #   {
  #     if(sum(sizediff==j)!=0) for(i in 1:sum(sizediff==j)) problemHHage[[paste0('sizedifferene',j)]][i] = (part.all$participant_age[sizediff==j][i] %in% hh_agex[sizediff==j,][i,])
  #     
  #   }
  
  sizediff>0
  
  positionoffirstmissingHH = as.integer(apply(hh_agex,1,function(x) which(is.na(x))[1]))
  for(i in 1:nrow(hh_agex))
  {
    if(sizediff[i]>0) hh_agex[i,positionoffirstmissingHH[i]] =  age_part[i]
  }
  rm(i,size,sizediff,sizereal,positionoffirstmissingHH,n)
}


if(GETHAM_POLYMOD)
{
  countrycode = as.character(sort(unique(part.all$country)))
  hh_age = hh_agex[,1:13]
  hh_age_g5=(as.matrix(hh_age)-1)%/%5+1
  
  
  
  HAM_POLYMOD$BEL = getHAMpolymod(COUNTRYCODE='BE')
  HAM_POLYMOD$DEU = getHAMpolymod(COUNTRYCODE='DE')
  HAM_POLYMOD$FIN = getHAMpolymod(COUNTRYCODE='FI')
  HAM_POLYMOD$GBR = getHAMpolymod(COUNTRYCODE='GB')
  HAM_POLYMOD$ITA = getHAMpolymod(COUNTRYCODE='IT')
  HAM_POLYMOD$LUX = getHAMpolymod(COUNTRYCODE='LU')
  HAM_POLYMOD$NLD = getHAMpolymod(COUNTRYCODE='NL')
  HAM_POLYMOD$POL = getHAMpolymod(COUNTRYCODE='PL')
  HAM_POLYMOD$EU = getHAMpolymod(COUNTRYCODE=countrycode)
  
}

rm(data.all,hh_age,hh_age_g5,hh_agex,part.all,age_part,age_partA5,CLEANHOUSEHOLD,countrycode,GETHAM_POLYMOD,READINDATA_POLYMOD,POLYMODcountries,POLYMODcountries_iso,getHAMpolymod)
save(HAM_POLYMOD,file = 'output/hampolymod.rdata')
