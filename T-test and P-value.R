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

