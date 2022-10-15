# load('input/pop/poptotal.rdata')
# source('compare_contact_surveys/codes/functions_processing.r')
synthetic = list()
empirical = list()
popage = list()

iso = 'CHN'
nage = 16
popage[[iso]] = as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
synthetic[[iso]] = normalize.contact.matrices(C = contacts[[iso]],popv=popage[[iso]],make.sym = FALSE)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_china),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]],make.sym = FALSE)


iso = 'FRA'
nage = 16
popage[[iso]] = as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
synthetic[[iso]] = normalize.contact.matrices(C = contacts[[iso]],popv=popage[[iso]],make.sym = FALSE)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_france),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]] ,make.sym = FALSE)


iso = 'HKG'
nage = 16
popage[[iso]] =as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
synthetic[[iso]] = normalize.contact.matrices(C = contacts[[iso]],popv=popage[[iso]],make.sym = FALSE)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_hongkong),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]] ,make.sym = FALSE)


iso = 'KEN'
nage = 5 # 7 age groups
popage[[iso]] = as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
synthetic[[iso]] = makeSyntheticLikeSurveyAge(iso='KEN',agebins = age_matrix$kenya,CONTACTS = contacts)
popken = c(as.numeric(poptotal[poptotal$iso3c %in% iso,4:4]),sum(as.numeric(poptotal[poptotal$iso3c %in% iso,5:6])),
           sum(as.numeric(poptotal[poptotal$iso3c %in% iso,7:7])),sum(as.numeric(poptotal[poptotal$iso3c %in% iso,8:14])),
           sum(as.numeric(poptotal[poptotal$iso3c %in% iso,15:19])))/poptotal$total[poptotal$iso3c %in% iso]
popage[['KEN_adjusted']] = popken
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_kenya),nrow = nage,ncol = nage,byrow = F)),popv = popken,make.sym = FALSE)
rm(iso,nage,popken)

iso = 'PER'
nage = 16
popage[[iso]] = as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
synthetic[[iso]] = normalize.contact.matrices(C = contacts[[iso]],popv=popage[[iso]],make.sym = FALSE)
index = which(is.na(matrix_peru))
matrix_peru[index] = 0
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_peru),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]] ,make.sym = FALSE)
empirical[[iso]]$all[index] = NA


iso = 'RUS'
nage = 15
popage[[iso]] =as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
poprus = c(as.numeric(poptotal[poptotal$iso3c %in% iso,4:17],sum(poptotal[poptotal$iso3c %in% iso,18:19])))/poptotal$total[poptotal$iso3c %in% iso]
popage[['RUS_adjusted']] = poprus
synthetic[[iso]] = makeSyntheticLikeSurveyAge(iso='RUS',agebins = age_matrix$russia,CONTACTS = contacts) 
index = which(is.na(matrix_russia))
matrix_russia[index] = 0
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_russia),nrow = nage,ncol = nage,byrow = F)),popv =poprus,make.sym = FALSE)
empirical[[iso]]$all[index] = NA
rm(iso,nage,poprus)


iso = 'ZAF'
nage = 10
popage[[iso]] =as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
popzaf = c(as.numeric(poptotal[poptotal$iso3c %in% iso,4:11],sum(poptotal[poptotal$iso3c %in% iso,12:19])))/poptotal$total[poptotal$iso3c %in% iso]
popage[['ZAF_adjusted']] = popzaf
synthetic[[iso]] = makeSyntheticLikeSurveyAge(iso='ZAF',agebins = age_matrix$southafrica,CONTACTS = contacts)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_southafrica),nrow = nage,ncol = nage,byrow = F)),popv = popzaf,make.sym = FALSE)
rm(iso,nage,popzaf)


iso = 'UGA'
nage = 9
popage[[iso]] =as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
popuga = c(as.numeric(poptotal[poptotal$iso3c %in% iso,4:6]),sum(as.numeric(poptotal[poptotal$iso3c %in% iso,7:8])),sum(as.numeric(poptotal[poptotal$iso3c %in% iso,9:10])),
           sum(as.numeric(poptotal[poptotal$iso3c %in% iso,11:12])),sum(as.numeric(poptotal[poptotal$iso3c %in% iso,13:14])), sum(as.numeric(poptotal[poptotal$iso3c %in% iso,15:16])),
           sum(as.numeric(poptotal[poptotal$iso3c %in% iso,17:19])))/poptotal$total[poptotal$iso3c %in% iso]
popage[['UGA_adjusted']] = popuga
synthetic[[iso]] = makeSyntheticLikeSurveyAge(iso='UGA',agebins = age_matrix$uganda,CONTACTS = contacts)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_uganda),nrow = nage,ncol = nage,byrow = F)),popv = popuga,make.sym = FALSE)
rm(iso,nage,popuga)


iso = 'VNM'
nage = 7 # 7 age groups
c(0,6,16,26,36,51,66,75)
popage[[iso]] =as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
popvnm = c(as.numeric(poptotal[poptotal$iso3c %in% iso,4:4]),sum(as.numeric(poptotal[poptotal$iso3c %in% iso,5:6])),
           sum(as.numeric(poptotal[poptotal$iso3c %in% iso,7:8])),sum(as.numeric(poptotal[poptotal$iso3c %in% iso,9:10])),
           sum(as.numeric(poptotal[poptotal$iso3c %in% iso,11:13])), sum(as.numeric(poptotal[poptotal$iso3c %in% iso,14:16])),
           sum(as.numeric(poptotal[poptotal$iso3c %in% iso,17:19])))/poptotal$total[poptotal$iso3c %in% iso]
popage[['VNM_adjusted']] = popvnm
synthetic[[iso]] = makeSyntheticLikeSurveyAge(iso='VNM',agebins = age_matrix$vietnam,CONTACTS = contacts)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_vietnam),nrow = nage,ncol = nage,byrow = F)),popv = popvnm,make.sym = FALSE)
rm(iso,nage,popvnm)

iso = 'ZWE'
nage = 16
popage[[iso]] =as.numeric(poptotal[poptotal$iso3c %in% iso,4:19]/poptotal$total[poptotal$iso3c %in% iso])
synthetic[[iso]] = normalize.contact.matrices(C = contacts[[iso]],popv=popage[[iso]],make.sym = FALSE)
empirical[[iso]] = normalize.contact.matrices(C = list(all = matrix(as.vector(matrix_zimbabwe),nrow = nage,ncol = nage,byrow = F)),popv =popage[[iso]] ,make.sym = FALSE)





