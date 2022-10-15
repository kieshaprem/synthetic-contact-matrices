library(tidyverse)

matrix1 <- matrix(runif(25), ncol=5) %>% signif(2) #random matrix
matrix2 <- (matrix1 + t(matrix1))/2 #symmetric matrix
image(matrix2)
image(matrix1)


matrix_sym <- function(mat){
  mat_sym <- (mat + t(mat))/2
  mat_anti<- (mat - t(mat))/2 
  ( norm(mat_sym, "f") - norm(mat_anti, "f") ) / ( norm(mat_sym, "f") + norm(mat_anti, "f") )
}

matrix_sym(matrix1)
matrix_sym(matrix2)

isolist = names(empirical)
isolist
ro=4
iso =isolist[ro];iso
n=ncol(empirical[[iso]]$all)
matrix_sym(empirical[[iso]]$all[1:n,1:n])
matrix_sym(synthetic2021[[iso]]$all[1:n,1:n])

symmetry = data.frame(iso = isolist,countryname = NA,empirical = NA ,synthetic = NA) 
symmetry$countryname = as.character(poptotal$countryname[poptotal$iso3c %in% symmetry$iso][c(1,3,2,4:10)])
symmetry$countryname[1] = 'Shanghai, China'
symmetry$countryname[3] = 'Hong Kong SAR, China'
for(ro in 1:10)
{
  iso =as.character(symmetry$iso[ro])
  n=ncol(empirical[[iso]]$all)
  symmetry$empirical[ro] = round(matrix_sym(empirical[[iso]]$all[1:n,1:n]),2)
  symmetry$synthetic[ro] = round(matrix_sym(synthetic2021[[iso]]$all[1:n,1:n]),2)
}

ro=5
iso =isolist[ro];iso
n=ncol(empirical[[iso]]$all)
symmetry$empirical[ro] =  round(matrix_sym(empirical[[iso]]$all[1:13,1:13]),2)
symmetry$synthetic[ro] =  round(matrix_sym(synthetic2021[[iso]]$all[1:13,1:13]),2)


write.csv(symmetry,file = 'output/symmetry.csv',row.names = FALSE)

