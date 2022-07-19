####ANOVA Test ####


## load required libraries
library(dplyr)


sepal_len_anova <- aov(data = iris, Sepal.Length ~ Species) #numeric variable ~ categorical variable

#Are any categories different?
summary(sepal_len_anova)
      #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Which categories are different?
TukeyHSD(sepal_len_anova)

#Answer: All of them!!

sepal_width_anova <- aov(data = iris, Sepal.Width ~ Species)
summary(sepal_width_anova)
TukeyHSD(sepal_width_anova)




#Let's look at diamond dataset
View(diamonds)
diamond_price_color <- aov(data = diamonds, price ~ color)
summary(diamond_price_color)
TukeyHSD(diamond_price_color)

#to save results, use $cat_ver
signif_results <- TukeyHSD(diamond_price_color)$color

#convert to dataframe so we can use dplyr 
arrange(as.data.frame(signif_results), `p adj`)


##Self practice
View(small_co2)
mean_co2_anova <- aov(data = small_co2, mc ~ Make)
summary(mean_co2_anova)
TukeyHSD(mean_co2_anova)
