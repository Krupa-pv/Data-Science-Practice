## Z scores and confidence intervals
## let's say we eat 100 boxes of cookies and find the average
## number of cookies in a box is 38.2

#### Z score test ####
sample_mean = 38.2
pop_mean = 40 #what our null hypothesis is testing against
sd = 10 #standard deviation
n = 100 # total sample side 


z = (sample_mean - pop_mean)/(sd/sqrt(n))

#whats the probablity if we took a random sample, we would come up with that mean

View(iris)
#one sample t-test
mean_sl <- mean(iris$Sepal.Length)
mean_sl

random_sample <- sample_n(iris, 30)
random_sample

#null hypothesis
  #random sample mean == population mean (5.84333)

#alternate hypothesis
  #random sample mean != population mean

random_sample_sl <- random_sample$Sepal.Length

t.test(mu = mean_sl, x = random_sample_sl)

setosa <- filter(iris, Species == "setosa")
setosa_sl <- setosa$Sepal.Length

t.test(mu = mean_sl, x = setosa_sl)

#Is the p-value less than 0.05?


####T-test####
sleep_hours <- c(5,5,5,6,6,7,7,7,8,9)
mean(sleep_hours)
sd(sleep_hours)

t.tesr(sleep_hours, mu = 7, alternative = "less")
#if one tailed t-test can add direction to see which way you want it to go

##Using iris dataset
pop_mean <- mean(iris$Sepal.Length)

setosa <- filter(iris, Species == "setosa")

t.test(setosa$Sepal.Length, mu = pop_mean)

#if the sample mean (of setosa sepal lengths) is 5.84(the same as the pop mean),
#then 0.00000000000000002% of the time, this diff (or more)
#will happen by chance

###Activity
Choose a different numeric variable (sepal width, petal length, petal width) and compare any of the 3 species to the population mean (setosa,) versicolor, or virginia

pop_mean2 <- mean(iris$Petal.Length)

versicolor <- filter(iris, Species == "versicolor")

t.test(versicolor$Petal.Length, mu = pop_mean2)

#Null - no difference b/w all petal length mean and versicolor petal length mean
#Alt - there is a difference


####2-sample T-test####
versicolor <- filter(iris, Species == "versicolor")
setosa <- filter(iris, Species == "setosa")

t.test(versicolor$Sepal.Length, setosa$Sepal.Length)

#(mean of setosa sepal lengths) - (mean of versicolor sepal lengths) !=0

#mean of setosa sepal lengths != mean of versicolor sepal lengths


##Compare versicolor and virginica sepal.lengths. What is the p-value? Is it significant?
virginica <- filter(iris, Species =="virginica")

t.test(virginica$Sepal.Length, versicolor$Sepal.Length)
      #p-value < 0.05 so yes it is significant


####Paired-t-test####
install.packages("datarium")
library(datarium)

t.test(mice2$before, mice2$after, paired = T)
#by doing pair it can do more detailed comparison rather than just mean
   #p-valye of 1.039e-09 is less than 0.05, so there IS a significant difference

#can also add directionality to paired t-test
t.test(mice2$before, mice2$after, paired = T, alternative = "less")
#if alternative = less, it is checking if after is greater than before. AKA before is less than after which is what we want to check









