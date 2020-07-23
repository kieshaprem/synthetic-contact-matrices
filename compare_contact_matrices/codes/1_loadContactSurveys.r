# devtools::install_github('sbfnk/socialmixr')
require(socialmixr)

downloadSurvey = TRUE

load('input/africanmatrices.rdata')
load('input/africanagematrices.rdata')

# This section of the codes requires stable internet connection.
if(downloadSurvey)
{
  china_survey <- get_survey("http://doi.org/10.5281/zenodo.3516113")
  france_survey <- get_survey("http://doi.org/10.5281/zenodo.1158452")
  hongkong_survey <- get_survey("http://doi.org/10.5281/zenodo.1165562")
  peru_survey <- get_survey("https://doi.org/10.5281/zenodo.1095664")
  russia_survey <- get_survey("http://doi.org/10.5281/zenodo.3415223")
  vietnam_survey <- get_survey("http://doi.org/10.5281/zenodo.1289474")
  # zambia_and_southafrica = get_survey('http://doi.org/10.5281/zenodo.2548693')
  # zimbabwe_survey <- get_survey("http://doi.org/10.5281/zenodo.1251944")
  data(polymod)
}


########################################################################
# Regrouping the age for the Vietnam survey 
# Levels: 0-5 6-15 16-25 26-34 35-49 50-64 65+
agechar = as.character(vietnam_survey$contacts$cnt_age_group)
agechar[agechar %in% "6-15"] ="06-15"
agechar[agechar %in% "0-5"] ="00-05"
vietnam_survey$contacts$cnt_age_exact = as.numeric(as.factor(agechar))
vietnam_survey$participants$part_age

agelevel = 1*(vietnam_survey$participants$part_age<6)+
  2*(vietnam_survey$participants$part_age>=6&vietnam_survey$participants$part_age<16)+
  3*(vietnam_survey$participants$part_age>=16&vietnam_survey$participants$part_age<26)+
  4*(vietnam_survey$participants$part_age>=26&vietnam_survey$participants$part_age<35)+
  5*(vietnam_survey$participants$part_age>=35&vietnam_survey$participants$part_age<50)+
  6*(vietnam_survey$participants$part_age>=50&vietnam_survey$participants$part_age<65)+
  7*(vietnam_survey$participants$part_age>=65)

vietnam_survey$participants$part_age = agelevel 
rm(agechar,agelevel)
########################################################################

getMatrix = function(df, co,nboot=50,agebins = seq(0,75,5))
{
  m <- contact_matrix(survey = df,countries = co, age.limits = agebins, n=nboot)
  mr <- Reduce("+", lapply(m$matrices, function(x) {x$matrix})) / length(m$matrices)
  image(mr)
  return(mr)
}


matrix_china = getMatrix(df = china_survey,co = 'Switzerland')
matrix_france = getMatrix(df = france_survey,co = 'France')
matrix_hongkong = getMatrix(df = hongkong_survey,co = 'Hong Kong')
matrix_peru = getMatrix(df = peru_survey,co = 'Peru')
matrix_russia = getMatrix(df = russia_survey,co = 'Russia')
matrix_vietnam = getMatrix(df = vietnam_survey,co = 'Vietnam',agebins = seq(1,7,1))
matrix_kenya = data.matrix(africanmatrices$kenya)
matrix_southafrica = data.matrix(africanmatrices$southafrica)
matrix_uganda = data.matrix(africanmatrices$uganda)
matrix_zimbabwe = data.matrix(africanmatrices$zimbabwe)



age_matrix = list(china = seq(0,75,5),france = seq(0,75,5), hongkong = seq(0,75,5), kenya = c(0,6,16,21,51,75), peru = seq(0,75,5),
                  russia = c(seq(0,70,5),80),southafrica = seq(0,50,5),uganda = africanagematrices$uganda,vietnam = c(0,6,16,26,36,51,66,75),zimbabwe = seq(0,75,5))

rm(pop,downloadSurvey,getMatrix,africanagematrices,africanmatrices)

save.image(file = 'input/contact_datasets.rdata')


