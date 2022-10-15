load('input/poptotal.rdata')
load('input/popUrban.rdata')
load('input/popRural.rdata')


synthetic2021 = list()
# synthetic2021_urban = list()
# synthetic2021_rural = list()
empirical = list()
popage = list()

# urban
iso = 'CHN'
nage = 16
# popage[[iso]] = as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
popage[[iso]] = (as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
  as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]
synthetic2021[[iso]] = normalize.contact.matrices(C = contacts2021_urban[[iso]],popv=popage[[iso]],make.sym = FALSE)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_china),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]],make.sym = FALSE)

# all 
iso = 'FRA'
nage = 16
popage[[iso]] = as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
synthetic2021[[iso]] = normalize.contact.matrices(C = contacts2021[[iso]],popv=popage[[iso]],make.sym = FALSE)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_france),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]] ,make.sym = FALSE)

# urban
iso = 'HKG'
nage = 16
popage[[iso]] = (as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
                   as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]
synthetic2021[[iso]] = normalize.contact.matrices(C = contacts2021_urban[[iso]],popv=popage[[iso]],make.sym = FALSE)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_hongkong),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]] ,make.sym = FALSE)

# rural
iso = 'KEN'
nage = 5 # 7 age groups
popage[[iso]] = (as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
                   as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]

synthetic2021[[iso]] = makeSyntheticLikeSurveyAge(iso='KEN',agebins = age_matrix$kenya,CONTACTS = contacts2021_rural)
popken = (c(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,4:4]),sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,5:6])),
            sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,7:7])),sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,8:14])),
            sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,15:19])))+
            c(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,4:4]),sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,5:6])),
              sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,7:7])),sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,8:14])),
              sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,15:19]))))/poptotal$total[poptotal$iso3c %in% iso]
# popken = popken/sum(popken)
popage[['KEN_adjusted']] = popken
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_kenya),nrow = nage,ncol = nage,byrow = F)),popv = popken,make.sym = FALSE)
rm(iso,nage,popken)

# rural 
iso = 'PER'
nage = 16
popage[[iso]] = (as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
                   as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]
synthetic2021[[iso]] = normalize.contact.matrices(C = contacts2021_rural[[iso]],popv=popage[[iso]],make.sym = FALSE)
index = which(is.na(matrix_peru))
matrix_peru[index] = 0
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_peru),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]] ,make.sym = FALSE)
empirical[[iso]]$all[index] = NA

# urban
iso = 'RUS'
nage = 15
popage[[iso]] = (as.numeric(popUrban$total_female[popUrban$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
                   as.numeric(popUrban$total_male[popUrban$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]
poprus = c(as.numeric(c(popUrban$total_female[popUrban$total_female$iso3c %in% iso,4:17],sum(popUrban$total_female[popUrban$total_female$iso3c %in% iso,18:19])))+
             as.numeric(c(popUrban$total_male[popUrban$total_male$iso3c %in% iso,4:17],sum(popUrban$total_male[popUrban$total_male$iso3c %in% iso,18:19]))))/poptotal$total[poptotal$iso3c %in% iso]
popage[['RUS_adjusted']] = poprus
synthetic2021[[iso]] = makeSyntheticLikeSurveyAge(iso='RUS',agebins = age_matrix$russia,CONTACTS = contacts2021_urban) 
index = which(is.na(matrix_russia))
matrix_russia[index] = 0
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_russia),nrow = nage,ncol = nage,byrow = F)),popv =poprus,make.sym = FALSE)
empirical[[iso]]$all[index] = NA
rm(iso,nage,poprus)

# rural 
iso = 'ZAF'
nage = 10
popage[[iso]] =(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
                  as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]
popzaf = (as.numeric(c(popRural$total_female[popRural$total_female$iso3c %in% iso,4:11],sum(popRural$total_female[popRural$total_female$iso3c %in% iso,12:19]))) + 
           as.numeric(c(popRural$total_male[popRural$total_male$iso3c %in% iso,4:11],sum(popRural$total_male[popRural$total_male$iso3c %in% iso,12:19]))))/poptotal$total[poptotal$iso3c %in% iso]
popage[['ZAF_adjusted']] = popzaf
# synthetic2021[[iso]] = makeSyntheticLikeSurveyAge(iso='ZAF',agebins = age_matrix$southafrica,CONTACTS = contacts2021_rural)
synthetic2021[[iso]] = makeSyntheticLikeSurveyAge(iso='ZAF',agebins =c(0,5,10,15,20,25,30,35,40,45,50),CONTACTS = contacts2021_rural)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_southafrica),nrow = nage,ncol = nage,byrow = F)),popv = popzaf,make.sym = FALSE)
rm(iso,nage,popzaf)

# rural
iso = 'UGA'
nage = 9
popage[[iso]] =(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
                as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]

popuga = (as.numeric(c(popRural$total_female[popRural$total_female$iso3c %in% iso,4:6],sum(popRural$total_female[popRural$total_female$iso3c %in% iso,7:8]),
                      sum(popRural$total_female[popRural$total_female$iso3c %in% iso,9:10]),sum(popRural$total_female[popRural$total_female$iso3c %in% iso,11:12]),
                      sum(popRural$total_female[popRural$total_female$iso3c %in% iso,13:14]), sum(popRural$total_female[popRural$total_female$iso3c %in% iso,15:16]),
                      sum(popRural$total_female[popRural$total_female$iso3c %in% iso,17:19])))+ 
  as.numeric(c(popRural$total_male[popRural$total_male$iso3c %in% iso,4:6],sum(popRural$total_male[popRural$total_male$iso3c %in% iso,7:8]),
               sum(popRural$total_male[popRural$total_male$iso3c %in% iso,9:10]),sum(popRural$total_male[popRural$total_male$iso3c %in% iso,11:12]),
               sum(popRural$total_male[popRural$total_male$iso3c %in% iso,13:14]), sum(popRural$total_male[popRural$total_male$iso3c %in% iso,15:16]),
               sum(popRural$total_male[popRural$total_male$iso3c %in% iso,17:19]))))/poptotal$total[poptotal$iso3c %in% iso]
popage[['UGA_adjusted']] = popuga
synthetic2021[[iso]] = makeSyntheticLikeSurveyAge(iso='UGA',agebins = age_matrix$uganda,CONTACTS = contacts2021_rural)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_uganda),nrow = nage,ncol = nage,byrow = F)),popv = popuga,make.sym = FALSE)
rm(iso,nage,popuga)

# rural
iso = 'VNM'
nage = 7 # 7 age groups
c(0,6,16,26,36,51,66,75)
popage[[iso]] =(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
                  as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]
popvnm = (c(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,4:4]),sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,5:6])),
           sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,7:8])),sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,9:10])),
           sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,11:13])), sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,14:16])),
           sum(as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,17:19])))+
            c(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,4:4]),sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,5:6])),
              sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,7:8])),sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,9:10])),
              sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,11:13])), sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,14:16])),
              sum(as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,17:19]))))/poptotal$total[poptotal$iso3c %in% iso]
popage[['VNM_adjusted']] = popvnm
synthetic2021[[iso]] = makeSyntheticLikeSurveyAge(iso='VNM',agebins = age_matrix$vietnam,CONTACTS = contacts2021_rural)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_vietnam),nrow = nage,ncol = nage,byrow = F)),popv = popvnm,make.sym = FALSE)
rm(iso,nage,popvnm)

# rural
iso = 'ZWE'
nage = 16
popage[[iso]] = (as.numeric(popRural$total_female[popRural$total_female$iso3c %in% iso,paste0('age',seq(0,75,5))][1,])+
                   as.numeric(popRural$total_male[popRural$total_male$iso3c %in% iso,paste0('age',seq(0,75,5))][1,]))/poptotal$total[poptotal$iso3c %in% iso]
synthetic2021[[iso]] = normalize.contact.matrices(C = contacts2021_rural[[iso]],popv=popage[[iso]],make.sym = FALSE)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_zimbabwe),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]] ,make.sym = FALSE)





