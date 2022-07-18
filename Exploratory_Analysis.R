## Practice plotting with your dataset
library(ggplot2)

ggplot(data = co2_data, aes(x=Make, y=CO2.Emissions.g.km., color=CO2.Emissions.g.km.)) +
  geom_point() 

ggplot(data = co2_data, aes(x=Fuel.Consumption.Comb..mpg., y=CO2.Emissions.g.km.)) +
  geom_line(stat="summary",
            fun = "mean")

##Practice subsetting data
#use a combination of filter, select, rename, mutate, arrange, summarize, group_by, sample, and/or slice
#create a visualization using your new subset of data

averaged_co2 <- co2_data %>%
  group_by(Make)%>%
  summarize(mean_co2 = mean(CO2.Emissions.g.km., na.rm = T),
            count = n()) %>%
  ggplot(aes(x=Make, y=mean_co2, color = Make)) + geom_point()+
  labs(title = "Average CO2 Emission per Make/Model of the Car")
averaged_co2




small_co2 <- summarize(group_by(co2_data, Make, Model), 
            mf = mean(Fuel.Consumption.Comb..mpg., na.rm = T), mc = mean(CO2.Emissions.g.km., na.rm = T))
small_co2
ggplot(small_co2, aes(x=mf, y=mc, color = Make))+
  geom_point() + 
  labs(x = "Mean Fuel Consumption for 100km", y = "Mean CO2 Emissions", 
       title = "CO2 Emissions per Fuel Consumption for 100km b/w different Makes")
 





