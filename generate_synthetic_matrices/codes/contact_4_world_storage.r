# 6 columns: ISO3C, overall/urban/rural, location of the contact, age group of contactor, age group of contactee, mean number of contact
library(data.table)

load('../compare_contact_matrices/input/synthetic_contacts.rdata')
overall = urban = rural = list()

for(i in 1:length(contacts2020))
{
  overall[[i]] = data.frame(iso3c = rep(names(contacts2020)[i],each = (5*16*16)),
                            setting = 'overall',
                            location_contact = rep(c('home','work','school','others','all'),each = 16*16),
                            age_contactor = rep(
                              rep(c('0 to 4','5 to 9','10 to 14','15 to 19','20 to 24','25 to 29','30 to 34','35 to 39','40 to 44','45 to 49','50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75+'),
                                  times = 16),times = 5),
                            age_cotactee = rep(
                              rep(c('0 to 4','5 to 9','10 to 14','15 to 19','20 to 24','25 to 29','30 to 34','35 to 39','40 to 44','45 to 49','50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75+'),
                                  each = 16),times = 5),
                            mean_number_of_contacts = c(as.vector(contacts2020[[i]]$home),
                                                        as.vector(contacts2020[[i]]$work),
                                                        as.vector(contacts2020[[i]]$school),
                                                        as.vector(contacts2020[[i]]$others),
                                                        as.vector(contacts2020[[i]]$all)))
}


for(i in 1:length(contacts2020_urban))
{
  urban[[i]] = data.frame(iso3c = rep(names(contacts2020_urban)[i],each = (5*16*16)),
                            setting = 'urban',
                            location_contact = rep(c('home','work','school','others','all'),each = 16*16),
                            age_contactor = rep(
                              rep(c('0 to 4','5 to 9','10 to 14','15 to 19','20 to 24','25 to 29','30 to 34','35 to 39','40 to 44','45 to 49','50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75+'),
                                  times = 16),times = 5),
                            age_cotactee = rep(
                              rep(c('0 to 4','5 to 9','10 to 14','15 to 19','20 to 24','25 to 29','30 to 34','35 to 39','40 to 44','45 to 49','50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75+'),
                                  each = 16),times = 5),
                            mean_number_of_contacts = c(as.vector(contacts2020_urban[[i]]$home),
                                                        as.vector(contacts2020_urban[[i]]$work),
                                                        as.vector(contacts2020_urban[[i]]$school),
                                                        as.vector(contacts2020_urban[[i]]$others),
                                                        as.vector(contacts2020_urban[[i]]$all)))
}

for(i in 1:length(contacts2020_rural))
{
  rural[[i]] = data.frame(iso3c = rep(names(contacts2020_rural)[i],each = (5*16*16)),
                          setting = 'rural',
                          location_contact = rep(c('home','work','school','others','all'),each = 16*16),
                          age_contactor = rep(
                            rep(c('0 to 4','5 to 9','10 to 14','15 to 19','20 to 24','25 to 29','30 to 34','35 to 39','40 to 44','45 to 49','50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75+'),
                                times = 16),times = 5),
                          age_cotactee = rep(
                            rep(c('0 to 4','5 to 9','10 to 14','15 to 19','20 to 24','25 to 29','30 to 34','35 to 39','40 to 44','45 to 49','50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75+'),
                                each = 16),times = 5),
                          mean_number_of_contacts = c(as.vector(contacts2020_rural[[i]]$home),
                                                      as.vector(contacts2020_rural[[i]]$work),
                                                      as.vector(contacts2020_rural[[i]]$school),
                                                      as.vector(contacts2020_rural[[i]]$others),
                                                      as.vector(contacts2020_rural[[i]]$all)))
}


syntheticcontacts_overall = rbindlist(overall)
syntheticcontacts_urban = rbindlist(urban)
syntheticcontacts_rural = rbindlist(rural)

synthetic_contacts_2020 = rbind(syntheticcontacts_overall,syntheticcontacts_urban,syntheticcontacts_rural)
write.csv(synthetic_contacts_2020,file = 'output/syntheticcontactmatrices2020/synthetic_contacts_2020.csv',row.names = FALSE)





