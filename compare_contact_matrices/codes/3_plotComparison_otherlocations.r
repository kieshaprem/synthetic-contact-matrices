

othercontacts = list()

othercontacts$POLYMOD = getOtherContactsProp(SURVEY = polymod)
othercontacts$CHN = getOtherContactsProp(SURVEY = china_survey)
othercontacts$FRA = getOtherContactsProp(SURVEY = france_survey)
othercontacts$HKG = getOtherContactsProp(SURVEY = hongkong_survey)
othercontacts$PER = getOtherContactsProp(SURVEY = peru_survey)
othercontacts$RUS = getOtherContactsProp(SURVEY = russia_survey)
othercontacts$ZWE = getOtherContactsProp(SURVEY = zimbabwe_survey)

if(SAVEPLOT) png('plots/compare_contacts_otherlocations.png',height=19,width=15,units='cm',res=1000,pointsize=10)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(2,3,0,0.5)))

  pushViewport(viewport(layout=grid.layout(nrow=3,ncol=2,width=c(rep(1,5)),height=c(rep(1,6)))))
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  plotOtherContacts(ISO = 'CHN',LOCATION = 'Shanghai, China',INDEX = 'A',YLAB=TRUE,XLAB=FALSE,LEGEND=FALSE)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
  plotOtherContacts(ISO = 'FRA',LOCATION = 'France',INDEX = 'B',YLAB=FALSE,XLAB=FALSE,LEGEND=TRUE)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
  plotOtherContacts(ISO = 'HKG',LOCATION = 'Hong Kong SAR, China',INDEX = 'C',YLAB=TRUE,XLAB=FALSE,LEGEND=FALSE)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
  plotOtherContacts(ISO = 'PER',LOCATION = 'Peru',INDEX = 'D',YLAB=FALSE,XLAB=FALSE,LEGEND=FALSE)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=3))
  plotOtherContacts(ISO = 'RUS',LOCATION = 'Russian Federation',INDEX = 'E',YLAB=TRUE,XLAB=TRUE,LEGEND=FALSE)
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=3))
  plotOtherContacts(ISO = 'ZWE',LOCATION = 'Zimbabwe',INDEX = 'F',YLAB=FALSE,XLAB=TRUE,LEGEND=FALSE)
  popViewport()
  
}
if(SAVEPLOT) dev.off()


