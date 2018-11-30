#libraries
library(naivebayes)
library(dplyr)
library(ggplot2)

#Getting the dataset
data <- read.csv("data_now.csv")
dim(data)
head(data)
str(data)
data[[50,1]] <- "patients"
data

#visualization
##Boxplot
data %>% ggplot(aes(x=vowel, y=f1, fill = condition)) +
  geom_boxplot() +
  ggtitle("f1 variation in healthy/patient classification")


data %>% ggplot(aes(x=vowel, y=f2, fill = condition)) +
  geom_boxplot() +
  ggtitle("f2 variation in healthy/patient classification")

data %>% ggplot(aes(x=vowel, y=f3, fill = condition)) +
  geom_boxplot() +
  ggtitle("f3 variation in healthy/patient classification")


library(phonR)

with(data, plotVowels(f1, f2, vowel, group = condition, plot.tokens = FALSE, plot.means = TRUE, pch.means = vowel, cex.means = 2, var.col.by = vowel, var.sty.by = condition, ellipse.line = TRUE, ellipse.fill = TRUE, fill.opacity = 0.4, poly.line = TRUE, poly.order = c("i", "e","a", "o", "u"), pretty = TRUE, legend.kwd = "bottomleft",legend.args = list(cex = 0.6)))  





library(dplyr)
data_i<-data%>%filter(vowel=="i")
str(data_i)

data_p<-data%>%filter(condition=='patients')
data_p
data%>%filter(sub_id=='p10')

data_h<-data%>%filter(condition=='healthy')
data_h
with(data_p, plotVowels(f1, f2, vowel, plot.tokens = FALSE, plot.means = TRUE, pch.means = vowel, cex.means = 2, var.col.by = vowel, var.sty.by = vowel, ellipse.line = TRUE, ellipse.fill = TRUE, fill.opacity = 0.4, poly.line = TRUE, poly.order = c("i", "e","a", "o", "u"), pretty = TRUE,
                        main = "vowel space (patients)",legend.kwd = "bottomleft",legend.args = list(cex = 0.6)))  
###### /i/ and /u/ formants are almost overlapped. /i/ is not at all front but produced towards the back of the mouth.
with(data_h, plotVowels(f1, f2, vowel, plot.tokens = FALSE, plot.means = TRUE, pch.means = vowel, cex.means = 2, var.col.by = vowel, var.sty.by = vowel, ellipse.line = TRUE, ellipse.fill = TRUE, fill.opacity = 0.4, poly.line = TRUE, poly.order = c("i", "e","a", "o", "u"), pretty = TRUE,
                        main = "vowel space (healthy)",legend.kwd = "bottomleft",legend.args = list(cex = 0.6)))  








#test and train(data= data)
dat <- sample(2,nrow(data), replace=TRUE, prob=c(0.8,0.2))
trainD <- data[dat==1,]
testD <- data[dat==2,]
dim(trainD)
dim(testD)

model<- naive_bayes(condition ~ ., data = trainD)
model

#predict
p <- predict(model, trainD, type = 'prob')
head(cbind(p, trainD))

#confusion matrix(train data)
p1 <- predict(model, trainD)
(tab1 <- table(p1, trainD$condition))
1 - sum(diag(tab1)) /sum(tab1)

#confusion matrix(test data)
p2 <- predict(model, testD)
(tab2 <- table(p2, testD$condition))
1 - sum(diag(tab2)) /sum(tab2)






