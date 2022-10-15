load('../output/syntheticmatrices/contact_all.rdata')
load('../output/syntheticmatrices/contact_home.rdata')
load('../output/syntheticmatrices/contact_work.rdata')
load('../output/syntheticmatrices/contact_school.rdata')
load('../output/syntheticmatrices/contact_others.rdata')

contacts2021 = list()
for(co in 1:length(contact_all))
{
  ISO = names(contact_all)[co]
  contacts2021[[ISO]]$home = contact_home[[ISO]]
  contacts2021[[ISO]]$work = contact_work[[ISO]]
  contacts2021[[ISO]]$school = contact_school[[ISO]]
  contacts2021[[ISO]]$others = contact_others[[ISO]]
  contacts2021[[ISO]]$all = contact_all[[ISO]]
}

rm(contact_all,contact_home,contact_work,contact_school,contact_others,co,ISO)

load('../output/syntheticmatrices/urban/contact_all_urban.rdata')
load('../output/syntheticmatrices/urban/contact_home_urban.rdata')
load('../output/syntheticmatrices/urban/contact_work_urban.rdata')
load('../output/syntheticmatrices/urban/contact_school_urban.rdata')
load('../output/syntheticmatrices/urban/contact_others_urban.rdata')

contacts2021_urban = list()
for(co in 1:length(contact_all))
{
  ISO = names(contact_all)[co]
  contacts2021_urban[[ISO]]$home = contact_home[[ISO]]
  contacts2021_urban[[ISO]]$work = contact_work[[ISO]]
  contacts2021_urban[[ISO]]$school = contact_school[[ISO]]
  contacts2021_urban[[ISO]]$others = contact_others[[ISO]]
  contacts2021_urban[[ISO]]$all = contact_all[[ISO]]
}

rm(contact_all,contact_home,contact_work,contact_school,contact_others,co,ISO)

load('../output/syntheticmatrices/rural/contact_all_rural.rdata')
load('../output/syntheticmatrices/rural/contact_home_rural.rdata')
load('../output/syntheticmatrices/rural/contact_work_rural.rdata')
load('../output/syntheticmatrices/rural/contact_school_rural.rdata')
load('../output/syntheticmatrices/rural/contact_others_rural.rdata')

contacts2021_rural = list()
for(co in 1:length(contact_all))
{
  ISO = names(contact_all)[co]
  contacts2021_rural[[ISO]]$home = contact_home[[ISO]]
  contacts2021_rural[[ISO]]$work = contact_work[[ISO]]
  contacts2021_rural[[ISO]]$school = contact_school[[ISO]]
  contacts2021_rural[[ISO]]$others = contact_others[[ISO]]
  contacts2021_rural[[ISO]]$all = contact_all[[ISO]]
}

rm(contact_all,contact_home,contact_work,contact_school,contact_others,co,ISO)

load('input/contacts.Rdata')
save.image(file = 'input/synthetic_contacts.rdata')
