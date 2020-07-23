# To save plots to a png file
SAVEPLOT = FALSE

# source the processing and plotting functions 
source('codes/functions_processing.r')
source('codes/functions_plot.r')

# load population age composition data 
# Ref: United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.
# Ref: Urban and Rural Population by Age and Sex, 1980-2015 United Nations, Department of Economic and Social Affairs, Population Division (2014). 
load('input/poptotal.rdata')
load('input/popUrban.rdata')
load('input/popRural.rdata')

########################################################################

# Section 1: Load the contact matrices
# load relevant the contact survey data files (from socialmixer + original references)
# need good internet connection: source('codes/1_loadContactSurveys.r')
load('input/contact_datasets.rdata')

# source the code to get the synthetic contact matrices 
source('codes/1_loadSyntheticContacts.r')
########################################################################

# Section 2: Normalise the matrices 

# The codes are sourced within the codes in section 3. 

# Normalise the matrices: matrix with all the contacts is normalized so that its dominant eigenvalue 
# is 1 and other matrices keep their contributions 
# ## Compare the relevant urban/rural matrices to the empirical matrices 
# URBANRURAL_ANALYSIS = TRUE
# ## Compare the 2017 matrices to the empirical matrices 
# COMPARE_2017_ANALYSIS = FALSE 
# source('codes/2_normalise.r')
########################################################################

# Section 3: Compare the matrices

# This code will plot the comparison of the normalised empirical and synthetic age-specific contact matrices (2020) in ten geographical regions. 
source('codes/3_plotComparison_urbanrural.r')

# This code will plot the comparison of the normalised empirical and synthetic age-specific contact matrices (2017) in ten geographical regions. 
source('codes/3_plotComparison_2017.r')

# This code will plot the comparison of the estimated proportion of contacts at other locations for the empirical contact studies from six geographical 
# regions and POLYMOD survey.
source('codes/3_plotComparison_otherlocations.r')
########################################################################

# Section 4 and 5: Impact of modelling physical distancing interventions for the COVID-19 pandemic using both the empirical and synthetic contact matrices

# This code will plot the comparison of the mean total number of contacts among children (0–9-year-olds) and older adults (60–69-year-olds), 
# as well as the basic reproduction number in rural and urban settings.
source('codes/4_plotImpact_R0_urbanrural.r')

# This code will plot the comparison of in relative reduction of COVID-19 cases between using the empirical and synthetic matrices in models of 
# COVID-19 epidemics in ten geographical regions
source('codes/5_plotImpact_interventionEffectiveness.r')

# This code will plot the comparison of in infecction attack rate for an unmitigated COVID-19 pandemic in ten geographical regions 
# using the empirical and synthetic matrices 
source('codes/5_plotImpact_infectionAttackRate.r')
########################################################################


