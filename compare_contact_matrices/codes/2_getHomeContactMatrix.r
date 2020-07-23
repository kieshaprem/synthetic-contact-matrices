

home = hhsize = list()

peru_survey$contacts = peru_survey$contacts[peru_survey$contacts$cnt_home %in% 1,]
home[['PER']] = (data.matrix(contact_matrix(peru_survey, countries = "Peru", age.limits = c(seq(0,80,5)))[[1]]))
agegp = peru_survey$participants$part_age %/% 5 +1
medianhhsize = array(NA,max(agegp,na.rm = TRUE))
for(age in 1:length(medianhhsize))
{
  medianhhsize[age] = median(peru_survey$participants$hh_size[agegp %in% age])
}
hhsize[['PER']] = medianhhsize
  
vietnam_survey$contacts = vietnam_survey$contacts[vietnam_survey$contacts$cnt_home %in% TRUE,]
home[['VNM']] = (data.matrix(contact_matrix(vietnam_survey, countries = "Vietnam", age.limits = c(seq(1,7,1)))[[1]]))

# zimbabwe_survey$contacts = zimbabwe_survey$contacts[zimbabwe_survey$contacts$cnt_home %in% TRUE,]
# home[['ZWE']] = (data.matrix(contact_matrix(zimbabwe_survey, countries = "Vietnam", age.limits = c(seq(1,7,1)))[[1]]))

save(home,file = 'compare_contact_surveys/output/home_contacts_empirical.rdata')
save(hhsize,file = 'compare_contact_surveys/output/hhsize_empirical.rdata')
