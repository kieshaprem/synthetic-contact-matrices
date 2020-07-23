load('../output/syntheticcontactmatrices2020/overall/contact_all.rdata')
load('../output/syntheticcontactmatrices2020/overall/contact_home.rdata')
load('../output/syntheticcontactmatrices2020/overall/contact_work.rdata')
load('../output/syntheticcontactmatrices2020/overall/contact_school.rdata')
load('../output/syntheticcontactmatrices2020/overall/contact_others.rdata')

contacts2020 = list()
for(co in 1:length(contact_all))
{
  ISO = names(contact_all)[co]
  contacts2020[[ISO]]$home = contact_home[[ISO]]
  contacts2020[[ISO]]$work = contact_work[[ISO]]
  contacts2020[[ISO]]$school = contact_school[[ISO]]
  contacts2020[[ISO]]$others = contact_others[[ISO]]
  contacts2020[[ISO]]$all = contact_all[[ISO]]
}

rm(contact_all,contact_home,contact_work,contact_school,contact_others,co,ISO)

load('../output/syntheticcontactmatrices2020/urban/contact_all_urban.rdata')
load('../output/syntheticcontactmatrices2020/urban/contact_home_urban.rdata')
load('../output/syntheticcontactmatrices2020/urban/contact_work_urban.rdata')
load('../output/syntheticcontactmatrices2020/urban/contact_school_urban.rdata')
load('../output/syntheticcontactmatrices2020/urban/contact_others_urban.rdata')

contacts2020_urban = list()
for(co in 1:length(contact_all))
{
  ISO = names(contact_all)[co]
  contacts2020_urban[[ISO]]$home = contact_home[[ISO]]
  contacts2020_urban[[ISO]]$work = contact_work[[ISO]]
  contacts2020_urban[[ISO]]$school = contact_school[[ISO]]
  contacts2020_urban[[ISO]]$others = contact_others[[ISO]]
  contacts2020_urban[[ISO]]$all = contact_all[[ISO]]
}

rm(contact_all,contact_home,contact_work,contact_school,contact_others,co,ISO)

load('../output/syntheticcontactmatrices2020/rural/contact_all_rural.rdata')
load('../output/syntheticcontactmatrices2020/rural/contact_home_rural.rdata')
load('../output/syntheticcontactmatrices2020/rural/contact_work_rural.rdata')
load('../output/syntheticcontactmatrices2020/rural/contact_school_rural.rdata')
load('../output/syntheticcontactmatrices2020/rural/contact_others_rural.rdata')

contacts2020_rural = list()
for(co in 1:length(contact_all))
{
  ISO = names(contact_all)[co]
  contacts2020_rural[[ISO]]$home = contact_home[[ISO]]
  contacts2020_rural[[ISO]]$work = contact_work[[ISO]]
  contacts2020_rural[[ISO]]$school = contact_school[[ISO]]
  contacts2020_rural[[ISO]]$others = contact_others[[ISO]]
  contacts2020_rural[[ISO]]$all = contact_all[[ISO]]
}

rm(contact_all,contact_home,contact_work,contact_school,contact_others,co,ISO)

load('input/contacts.Rdata')
save.image(file = 'input/synthetic_contacts.rdata')
