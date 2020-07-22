library(countrycode)
readInRaw =TRUE

if(readInRaw){
  clusterdata = clusterdataOriginal = read.csv('input/clusterPOLYMOD.csv',as.is = T)
  load(file = 'input/wbindicators.rdata')
  load('output/hamdhs.rdata')
  load('output/hampolymod.rdata')
}
rm(readInRaw)

##############################################################################################################
### Preparation
# Preparing the data matrix before computing correlation

# clusterdata = clusterdataOriginal[,c(1,6:12,16:17)]
# iso = countrycode(clusterdata[,1],origin = 'country.name',destination = 'iso3c')
# clusterdata = clusterdata[!(is.na(iso)),]
# iso = iso[!(is.na(iso))]
# iso[156] = 'PRK'
# row.names(clusterdata) = iso 
# clusterdata = clusterdata[,-1]

clusterdata = wbindicators
row.names(clusterdata) = clusterdata[,1]
# clusterdata = (clusterdata[,-c(1,7,15)])
clusterdata = (clusterdata[,c(10,4,3,2,14,5,27,28,26,25,24,23,22,16)])

clusterdata = as.matrix(clusterdata)
summary(clusterdata)
class(clusterdata)

# check missingness of data
table(apply(clusterdata,1, function(x) sum(is.na(x))))
apply(clusterdata,1, function(x) sum(is.na(x)))[apply(clusterdata,1, function(x) sum(is.na(x)))>6]
clusterdata = clusterdata[(apply(clusterdata,1, function(x) sum(is.na(x)))<6),]

##############################################################################################################
### Standardisation 
# centering each variable to have mean 0 and scaling each variable to have standard deviation 1. 

clusterdataZ = apply(clusterdata,2,function(x){(x-mean(x,na.rm = T))/sd(x,na.rm = T)})
clusterdataZT = t(clusterdataZ)
clusterdataZT
pars = ncol(clusterdataZ)
par(mfrow = c(5, 5))
for(i in 1:pars) hist(clusterdataZT[i,],freq = T, xlab = row.names(clusterdataZT)[i],main =  paste('Standardized',row.names(clusterdataZT)[i]),col='grey50',border = 'white')
par(mfrow = c(1, 1))



##############################################################################################################
### Testing with obvious countries: Austria and Germany are (quite) similar countries.

clusterdataZT[,countrycode('Austria',origin = 'country.name',destination =  'iso3c')] 
clusterdataZT[,countrycode('Germany',origin = 'country.name',destination =  'iso3c')] 
cor(clusterdataZT[,'AUT'] ,clusterdataZT[,'DEU'] )
cov(clusterdataZT[,'AUT'] ,clusterdataZT[,'DEU'] )

##############################################################################################################
### covariance and correlation pair matrix 
# 

covpair = corpair= array(NA,c(ncol(clusterdataZT),ncol(clusterdataZT)))
row.names(covpair) = row.names(corpair) = colnames(clusterdataZT)
colnames(covpair) = colnames(corpair) = colnames(clusterdataZT)

for(i in 1:ncol(clusterdataZT)){
  for(j in 1:ncol(clusterdataZT)){
    covpair[i,j] = cov(clusterdataZT[,i] ,clusterdataZT[,j],use = 'na.or.complete' )
    corpair[i,j] = cor(clusterdataZT[,i] ,clusterdataZT[,j],use = 'na.or.complete' )
  }
}

POLYMODcountries = names(HAM_POLYMOD)[-9] 
DHScountries = names(HAM_DHS)
countries = c(POLYMODcountries,DHScountries)


correlation_values = apply(corpair[countries,-which(colnames(corpair) %in% c(countries))],2,function(x){max(x)})
similarity = apply(corpair[countries,-which(colnames(corpair) %in% c(countries))],2, function(x) {which.max(x)})

data.frame(ROW = countrycode(names(similarity),origin = 'iso3c',destination = 'country.name'),
           POLYMODDHS= countrycode(countries[as.numeric(similarity)],origin = 'iso3c',destination = 'country.name'),
           correlation_values)

matched = data.frame(names(similarity),countries[as.numeric(similarity)],as.numeric(as.vector(correlation_values)))
colnames(matched) = c("country","matched" ,"correlation_values")
hist(matched$correlation_values,freq = F)
# write.csv(matched, 'correlationvalues.csv',row.names = FALSE)


distancemat = array(NA,c(nrow(clusterdataZ),length(countries)))
colnames(distancemat) = rownames(clusterdataZ)[rownames(clusterdataZ) %in% countries]
for(c in 1:length(countries)){
  
  distancemat[,c] = apply(clusterdataZ, 1, function(x) dist(rbind(x,clusterdataZ[which(rownames(clusterdataZ) %in% countries)[c],])))
}

countries[apply(distancemat,1,which.min)]
rownames(distancemat) = rownames(clusterdataZ)

distancemat['AUT',]
distancemat['DEU',]
distancemat['AFG',]
distancemat['AGO',]
# save(distancemat,file = 'output/distancemat.RData')


pars = ncol(clusterdataZ)
timestamp()
B=10000
wbootstrap = list()

for(COUNTRYindex in 1:nrow(clusterdataZ)){
  
  wsampled = array(NA,c(B,length(countries))) 
  
  for(polymoddhs in 1:length(countries)){
  
    temp=clusterdataZ[c(COUNTRYindex,which(rownames(clusterdataZ) == countries[polymoddhs])),]
    diff2 =(temp[1,]-temp[2,])^2
    for(b in 1:B)
    {
      wsampled[b,polymoddhs]=sqrt(sum(sample(diff2,pars,replace=TRUE),na.rm = T))
    }
  }
  colnames(wsampled) = countries
  wbootstrap[[rownames(clusterdataZ)[COUNTRYindex]]] = wsampled 
  print(COUNTRYindex)
}
timestamp()
save(wbootstrap,file='output/HAMweightsbootstrap.rdata')


