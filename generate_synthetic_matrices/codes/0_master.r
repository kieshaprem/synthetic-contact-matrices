# Master code 
# Extention of  POLYMOD Contact data to non-POLYMOD countries



## POPULATION AGE STRUCTURE----------------------------------------------------------------- 
# output: 'input/pop/poptotal.rdata' and 'input/pop/popratio.rdata'
# Ref: United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.
loadPopAge = FALSE
if(loadPopAge) source('codes/pop_getPopAge.r')
if(loadPopAge) source('codes/pop_getPopAgeUrbanRural.r')

# Population socio-demographic factors for most countries of the world from World Bank and other online database 
loadPopIndicators = FALSE
if(loadPopIndicators) source('codes/pop_getWBindicators.r')



## HOME CONTACTS---------------------------------------------------------------------------- 

# Get Household Age Matrix (HAM) for POLYMOD and DHS countries and the other countries of the world using the Population Ratio model. 
# The household age matrix (HAM) for country c, (h_(a,\alpha)^c ), equal to the mean number of household members of age \alpha 
# of an individual aged a. 
# Ref: https://dhsprogram.com/data/ 
# output: 'output/hampolymod.rdata' and 'output/hamdhs.rdata'  
# Both files are lists containing the HAM of POLYMOD and DHS countries.

loadHHdhs = FALSE
getHHdhsSummary = FALSE
loadHHpolymod = FALSE
buildPopRatioModel = FALSE
getBootstrapSamples = FALSE
getHAMweights = FALSE
getHAMworld = FALSE
validateHAM = FALSE

# computationally intensive and requires household data from The DHS program: if(loadHHdhs) source('codes/home_0_getHamDHS.r')
# computationally intensive and requires household data from The DHS program: if(loadHHdhs) source('codes/home_0_getHamDHS_urbanrural.r')
# computationally intensive and requires household data from The DHS program: if(getHHdhsSummary) source('codes/home_0_getDHSsummary.r')
# requires household data from The DHS program: if(loadHHpolymod) source('codes/home_0_getHamPOLYMOD.r')
if(buildPopRatioModel) source('codes/home_1_popRatioModel.r')
if(buildPopRatioModel) source('codes/home_1_popRatioModel_urbanrural.r')
if(getBootstrapSamples) source('codes/home_2_getHAMweightsBootstrapsamples.r')
if(getHAMweights) source('codes/home_3_getHAMweights.r')
if(getHAMweights) source('codes/home_3_getHAMweights_urbanrural.r')
if(getHAMworld) source('codes/home_4_getHAMworld.r')
if(getHAMworld) source('codes/home_4_getHAMworld_urbanrural.r')
if(validateHAM) source('codes/home_validation_internal_HAM.r')
# requires household data from The DHS program: if(validateHAM) source('codes/home_validation_getHHsizebyage.r')


## WORK CONTACTS---------------------------------------------------------------------------

# To build the working population age structure, W_(a,\alpha)^c, for country c.  
# The labour force participation rate by sex and 5-year age groups for most countries of the world 
# were obtained from the International Labor Organization (ILO) on-line database.
# input : 'input/work/labour_force_participation_rate_ILO_modelled_estimates.csv' #extracted from ILO database
# output: 'input/work/workpopage.rdata'
# 'workpopage' is a list containing the working population age structure for (1) total, (2) females, and (3) males.
loadWorkPop = FALSE
testWorkPop = FALSE

if(loadWorkPop) source('codes/work_1_buildWorkingPopulation.r')
if(testWorkPop) source('codes/work_2_test.r')




## SCHOOL CONTACTS----------------------------------------------------------------- 

# To build the school population age structure, S_(a,\alpha)^c, for country c.
# 1) The net enrolment rates of school-going individuals at different school levels (pre-primary, primary, secondary, tertiary)
# for most countries of the world
# 2) The pupil-teacher ratio at different levels (pre-primary, primary, secondary, tertiary) for most countries of the world
# 3) The number of secondary school teachers for most countries of the world to include both full-time and part-time teachers (to determine the adjustment factor)
# 4) The age of teachers by school level, to assign teachers to the appropriate age bins of working-age individuals
# After loading all the relevant data sets, estimate the number of students, then teachers, before building the school population age structure.
# output: 'input/school/schoolage.rdata'
loadSchoolEnrolment = FALSE
loadSchoolPTR = FALSE
loadSchoolSecTeachers = FALSE
loadSchoolTeacherAgeLevel = FALSE
getSchoolStudents = FALSE
getSchoolTeachers = FALSE
getSchoolPop = FALSE

if(loadSchoolEnrolment) source('codes/school_0_enrolment.r')
if(loadSchoolPTR) source('codes/school_0_pupilToTeacherRatio.r')
if(loadSchoolSecTeachers) source('codes/school_0_numberSecTeachers.r')
if(loadSchoolTeacherAgeLevel) source('codes/school_0_teacherAgeLevel.r')

if(getSchoolStudents) source('codes/school_1_calculateStudents.r')
if(getSchoolTeachers) source('codes/school_2_calculateTeachers.r')
if(getSchoolPop) source('codes/school_3_buildSchoolPopulation.r')

## SYNTHETIC CONTACT MATRICES----------------------------------------------------------------- 

# Load outputs from the Hierarchical model of the POLYMOD contact survey
# source('codes/contact_1_polymod.r')
# Build the population age structure at home, work, and school and extrapolate their contact patterns 
source('codes/contact_1_world.r')
source('codes/contact_2_world_rural.r')
source('codes/contact_2_world_urban.r')
# plot the contact matrices 
source('codes/contact_3_world_plot.r')
source('codes/contact_3_world_plot_rural.r')
source('codes/contact_3_world_plot_urban.r')

##----------------------------------------------------------------------------------------------------------------------------------