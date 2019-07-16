library(magrittr); library(dplyr)
dipole_moments <- read.csv('./ScalarCoupling/data/dipole_moments.csv', stringsAsFactors = F)
dipole_moments %>% str
dipole_moments %>% summary
dipole_moments[,2:4] %>% pairs
dipole_moments[,2:4] %>% cor

magnetic <- read.csv('./ScalarCoupling/data/magnetic_shielding_tensors.csv', stringsAsFactors = F)
magnetic %>% str
magnetic %>% summary
magnetic[,2:11] %>% pairs
magnetic[,2:11] %>% cor

mulliken <- read.csv('./ScalarCoupling/data/mulliken_charges.csv', stringsAsFactors = F)
mulliken %>% str
mulliken %>% summary
mulliken[,2:3] %>% pairs
mulliken[,2:3] %>% cor

potential <- read.csv('./ScalarCoupling/data/potential_energy.csv', stringsAsFactors = F)
potential %>% str
potential %>% summary
potential$potential_energy %>% hist
