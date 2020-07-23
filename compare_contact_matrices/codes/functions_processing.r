
normalize.contact.matrices = function(C, popv, make.sym = F){
  # FUN normalize them so that
  # the matrix with all the contacts is normalized so that its dominant eigenvalue is 1 
  # and other matrices keep their contributions 
  if (make.sym){
    Csym <- lapply(C, function(x, popv) (x + t(x)*((popv)%*%t(1/popv)))/2, popv) # make sure contacts are reciprocal
  } else {
    Csym <- C # if make.sym = F leave it as is
  }
  eig1 <- Re(eigen(Csym["all"]$all)$value[1])  # save dominant eigenvalue of the matrix with all contacts
  
  # divide all matrices by the real part of the dominant matrix of all of the contacts
  # that way all the rescaled matrices still sum to C_all = C_work + C_home + C_school + C_other
  Cnorm <- lapply(Csym, function(x,eig1) x/eig1, eig1)
  
  return(Cnorm)
}

# Agegroups for the Vietnam survey: 0-5 6-15 16-25 26-34 35-49 50-64 65+
makeSyntheticLikeSurveyAge=function(iso,agebins,CONTACTS=contacts2020)
{
  C = normalize.contact.matrices(C = CONTACTS[[iso]],popv =as.numeric(pop2020[pop2020$iso3c %in% iso,7:22]),make.sym = FALSE)
  newMat = matrix(data = NA,nrow = length(agebins)-1,ncol = length(agebins)-1)
  mat = list(home = newMat,work = newMat, school= newMat, others = newMat, all = newMat)
  index = agebins%/%5+1
  for(l in 1:5)
  {
    for(j in 1:(length(agebins)-1)) 
    {
      for(i in 1:(length(agebins)-1))
      {
        C_section = (C[[l]][index[i]:(index[i+1]-1),index[j]:(index[j+1]-1)])
        if(length(index[i]:(index[i+1]-1))==1 & length(index[j]:(index[j+1]-1))==1)  mat[[l]][i,j] = C_section
        if(length(index[i]:(index[i+1]-1))==1 & length(index[j]:(index[j+1]-1))> 1)  mat[[l]][i,j] = sum((C_section))
        if(length(index[i]:(index[i+1]-1))> 1 & length(index[j]:(index[j+1]-1))==1)  mat[[l]][i,j] = mean((C_section))
        if(length(index[i]:(index[i+1]-1))> 1 & length(index[j]:(index[j+1]-1))> 1)  mat[[l]][i,j] = mean(rowSums(C_section))
      }
    }
    
  }
  # image(newMat)
  return(mat)
}

calculateCI <- function (X, conf.level=0.95) {
  alpha = 1 - conf.level
  upper <- 0.5 * qchisq(1-alpha/2, 2*X+2)
  lower <- 0.5 * qchisq(alpha/2, 2*X)
  return(c(lower, upper))
}

exactPoiCI <- function (X, conf.level=0.95) {
  alpha = 1 - conf.level
  upper <- 0.5 * qchisq(1-alpha/2, 2*X+2)
  lower <- 0.5 * qchisq(alpha/2, 2*X)
  return(c(lower, upper))
}

getOtherContactsProp = function(SURVEY)
{
  names_allcontact = c('cnt_home','cnt_work','cnt_school','cnt_transport','cnt_leisure','cnt_otherplace','cnt_otherpublicplace','cnt_other')
  names_othercontact = c('cnt_transport','cnt_leisure','cnt_otherplace','cnt_otherpublicplace','cnt_other')
  SURVEY$participants = data.frame(SURVEY$participants)
  SURVEY$contacts = data.frame(SURVEY$contacts)
  SURVEY$contacts =  SURVEY$contacts[,colnames(SURVEY$contacts) %in% c('part_id',names_allcontact)]
  # sum(is.na(match(SURVEY$contacts$part_id,SURVEY$participants$part_id)))
  SURVEY$contacts$all_contacts = rowSums(SURVEY$contacts[,colnames(SURVEY$contacts) %in% c(names_allcontact)],na.rm = TRUE)
  SURVEY$contacts$other_contacts = rowSums(SURVEY$contacts[,colnames(SURVEY$contacts) %in% c(names_othercontact)],na.rm = TRUE)
  
  others_contacts = data.frame(part_id = unique(sort(SURVEY$participants$part_id)), 
                               part_age = NA, part_age_gp = NA,
                               cnt_all = 0, 
                               cnt_others = 0)
  others_contacts$part_age = SURVEY$participants$part_age[match(others_contacts$part_id,SURVEY$participants$part_id)]
  others_contacts$part_age_gp = others_contacts$part_age %/% 5 +1 
  aggregatecontacts = aggregate(x = SURVEY$contacts$all_contacts, by =list(SURVEY$contacts$part_id),FUN=sum)
  others_contacts$cnt_all = aggregatecontacts$x[match(others_contacts$part_id,aggregatecontacts$Group.1)]
  rm(aggregatecontacts)
  aggregatecontacts = aggregate(x = SURVEY$contacts$other_contacts, by =list(SURVEY$contacts$part_id),FUN=sum)
  others_contacts$cnt_others = aggregatecontacts$x[match(others_contacts$part_id,aggregatecontacts$Group.1)]
  rm(aggregatecontacts)
  
  others_contacts$prop_others = others_contacts$cnt_others/others_contacts$cnt_all
  
  
  summary_others = data.frame(part_age_gp = seq(1,max(others_contacts$part_age_gp,na.rm = TRUE),1),
                              n = tabulate(others_contacts$part_age_gp),
                              mean = NA,
                              sd = NA, 
                              median = NA, 
                              q1 = NA, 
                              q3 = NA)
  
  aggregatecontacts = aggregate(x = others_contacts$prop_others, by =list(others_contacts$part_age_gp),FUN=function(x) mean(x,na.rm = TRUE))
  summary_others$mean = aggregatecontacts$x[match(summary_others$part_age_gp,aggregatecontacts$Group.1)]
  rm(aggregatecontacts)
  aggregatecontacts = aggregate(x = others_contacts$prop_others, by =list(others_contacts$part_age_gp),FUN=function(x) sd(x,na.rm = TRUE))
  summary_others$sd = aggregatecontacts$x[match(summary_others$part_age_gp,aggregatecontacts$Group.1)]
  rm(aggregatecontacts)
  aggregatecontacts = aggregate(x = others_contacts$prop_others, by =list(others_contacts$part_age_gp),FUN=function(x) median(x,na.rm = TRUE))
  summary_others$median = aggregatecontacts$x[match(summary_others$part_age_gp,aggregatecontacts$Group.1)]
  rm(aggregatecontacts)
  
  aggregatecontacts = aggregate(x = others_contacts$prop_others, by =list(others_contacts$part_age_gp),FUN=function(x) quantile(x,probs = 0.25,na.rm = TRUE))
  summary_others$q1 = aggregatecontacts$x[match(summary_others$part_age_gp,aggregatecontacts$Group.1)]
  rm(aggregatecontacts)
  aggregatecontacts = aggregate(x = others_contacts$prop_others, by =list(others_contacts$part_age_gp),FUN=function(x) quantile(x,probs = 0.75,na.rm = TRUE))
  summary_others$q3 = aggregatecontacts$x[match(summary_others$part_age_gp,aggregatecontacts$Group.1)]
  rm(aggregatecontacts)
  
  summary_others$lower = summary_others$mean - 1.96*(summary_others$sd/sqrt(summary_others$n))
  summary_others$upper = summary_others$mean + 1.96*(summary_others$sd/sqrt(summary_others$n))
  
  
  plot(summary_others$part_age_gp,summary_others$mean,ylim = c(0,1),pch=16,ylab = 'Proportion of contacts at other locations',xlab = 'Age group of individuals')
  for(i in 1:16) lines(c(summary_others$part_age_gp[i],summary_others$part_age_gp[i]),c(summary_others$lower[i],summary_others$upper[i]))
  points(summary_others$part_age_gp,summary_others$median,pch=6,col='red')
  return(summary_others)
}
