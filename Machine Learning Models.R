## load required libraries
library(dplyr)
library(ggplot2)

##Unsupervised learning: k-means clustering

iris_numerics <- select(iris, -Species)%>%
  scale() #so scale basically sets everything in relation to the mean
iris_numerics
summary(iris_numerics)


iris_clusters <- kmeans(iris_numerics, centers = 3)
iris_clusters #our results


iris_clusters$cluster #vector designating a cluster for each row
iris$cluster <- iris_clusters$cluster #add cluster column to original dataset


ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point(aes(color = as.factor(cluster)))


##Practice
View(small_co2)
co2_numerics <- select(co2_data, Make, CO2.Emissions.g.km.)
co2_clusters <- kmeans(co2_numerics, centers )


###Supervised modeling
#Visualizing data
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point()
ggplot(iris, aes(Petal.Length, y = Petal.Width)) + geom_point()
ggplot(iris, aes(Petal.Width, y = Sepal.Length)) + geom_point()
cor(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)
cor(iris$Petal.Width, iris$Sepal.Length)
cor(iris$Petal.Length, iris$Sepal.Length)
#-1 and 1 means good correlation. -1 is neg cor, 1 is pos cor


##Choose features
#Sepal.Length, Petal.Width, Sepal.Width


#Split into training, test, and validation sets
greetings <- c(rep("hello", 5), rep("goodbye", 3))%>%
  sample(8, replace = F) #replace = F takes out whatever you already used
greetings

iris_len <- nrow(iris)

iris$label <- c(rep("training", ceiling(0.6*iris_len)), 
                    rep("test", ceiling(0.2*iris_len)),
                    rep("validation", ceiling(0.2*iris_len))) %>%
  sample(iris_len, replace = F)

head(iris)
#ceiling rounds up
  
  
###Choosing a model!
#when we use a model, we train it using the training set and 
#"test' it using the testing set

iris_train <- filter(iris, label == "training")
iris_test <- filter (iris, label=="test")
iris_valid <- filter(iris, label =="validation")

##Linear model

iris_lm<- lm(Petal.Length ~ Petal.Width +Sepal.Length + Sepal.Width, 
             data = iris_train)
iris_lm
?lm()
#select out only the x values we use (Petal.Width and Sepal.Length)
iris_lm_prediction <- select(iris_test, Petal.Width, Sepal.Length, Sepal.Width) %>%
  predict(object = iris_lm) #object is the model that we just created

iris_test$lm_pred <- iris_lm_prediction

head(iris_test)



####Logistic Model####

mean(iris$Petal.Length)
iris_train_glm <- iris_train %>% 
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length < 3.758, "short", "long"))) #short if T, long if F

head(iris_train_glm)

iris_glm <- glm(petal_length_cat ~ Petal.Width + Sepal.Length + Sepal.Width, 
                data = iris_train_glm,
                family = binomial(link = "logit"))

iris_glm_preds <- iris_test %>%
  select(Petal.Width, Sepal.Length, Sepal.Width, Species) %>%
  predict(object = iris_glm)

iris_test$glm_pred <- iris_glm_preds

iris_test <- iris_test %>%
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length < 3.758, "short", "long")))
iris_test

# filter down to 2 categories
iris_train_2species <- filter(iris_train, Species %in% c("setosa", "virginica"))
iris_train_2species

# create the model
iris_glm <- glm(Species ~ Sepal.Width+ Sepal.Length+ Petal.Width+ Petal.Length, 
                data = iris_train_2species,
                family = binomial(link = "logit"))
summary(iris_glm)

#make predictions based on model
iris_2species_preds <- iris_test %>% 
  filter(Species %in% c("setosa", "virginica"))

iris_2species_preds <- iris_test_2species %>%
  select(-Species)%>%
           predict (object = iris_glm)

iris_test_2species$glm_2spec_pred <- iris_2species_preds



####Generalized Boosted Regression Modeling####
install.packages("gbm")
library(gbm)

#create the model
iris_gbm <- gbm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species,
                data = iris, #iris_test was too small, make sure to use test for your data
                n.trees = 500) 
summary(iris_gbm)

#select out only the x values we used from test and predict
iris_gbm_preds <- iris_test %>%
  select(Petal.Width, Sepal.Length , Sepal.Width, Species) %>%
  predict(object = iris_gbm)

#save predictions based into test set
iris_test$gbm_pred <- iris_gbm_preds


## Evaluate performance of models
View(iris_test)

install.packages("Metrics")
library(Metrics)

#calculate rmse between predictions and true values
rmse(iris_test$Petal.Length, iris_test$lm_pred) 
rmse(iris_test$Petal.Length, iris_test$gbm_pred)#wins! smaller error

iris_test$gbm_pred

#calculate mae between predictions and true values
mae(iris_test$Petal.Length, iris_test$lm_pred) 
mae(iris_test$Petal.Length, iris_test$gbm_pred)#wins! smaller error

# Accuracy (checks how much is right)
iris_test <- iris_test %>%
  mutate(glm_petal_cat = ifelse(glm_pred < 0, "long", "short"))
View(iris_test)

true_vals <- sum(iris_test$glm_petal_cat == iris_test$petal_length_cat)
total_vals <- nrow(iris_test)

accuracy <- true_vals/total_vals
accuracy



#f1 score tells us about false positive & false negative rates (checks how much is wrong)

f1(iris_test$glm_petal_cat, iris_test$petal_length_cat)

#save predictions back into test set
iris_test$gbm_pred <- iris_gbm_preds

View(iris_test)
