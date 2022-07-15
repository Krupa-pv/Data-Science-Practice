#### Load in required libraries ####
library(ggplot2)

#### Load in csv file ####
co2_data <- read.csv("data/CO2_Emissions_Canada.csv.xls")
View(co2_data)

#### Save R object as a file ####
saveRDS(co2_data, "data/co2_data.RDS")

