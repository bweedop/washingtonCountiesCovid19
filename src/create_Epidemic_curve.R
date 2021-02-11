# Script to plot epidemic curve
## traditional and time-lapse map


# libraries

my.packages <- c("readxl", "dplyr", "magrittr", "usmap", "ggplot2")

lapply(my.packages, library, character.only = TRUE)



# import data

population <- read_xlsx("./data/rawData/PopulationData/co-est2019-annres-53.xlsx", skip = 3, n_max = 40)

case <- read_xlsx("./data/rawData/COVID-19CaseData/WA_COVID19_Cases_Hospitalizations_Deaths.xlsx")


# clean

population$county <- sapply(population[,1], function(x){gsub("^[\\.]([^,]+)[,].*", "\\1", x)})
