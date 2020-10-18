install.packages("Rcpp")
install.packages("dplyr")
if(!require(devtools)) install.packages("devtools")
install.packages("ggpubr")

library("ggpubr")
library("car")

data <- read.csv(file.choose())

#Giving labels to categorical data in nmeric form
data$gender <- factor(data$gender, 
                    levels = c(0, 1),
                    labels = c("Female", "Male"))
head(data)

#Making box plots of the data for outlier detection
ggboxplot(data, x = "Diet", y = "weightlost", color = "gender", palette = c("#00AFBB", "#E7B800"))


#We now make interaction plot to get an idea whether interaction effects are significant
interaction.plot(x.factor = data$Diet, trace.factor = data$gender, 
                 response = data$weightlost, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Diet Type", ylab="Weight Lost",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))


#For checking homogenity of variance assumption, we use Levene test.p value<0.05 
#shows the variance across groups is statistically significantly different.
leveneTest(weightlost~as.factor(gender)*as.factor(Diet),data = data)

#For checking normality assumption, we use Shapiro Wilk test.p value>0.0.5 for test on residuals indicates normal behaviour
#For also use the plot of residuals for the same
anova2 <- aov(weightlost~as.factor(gender)*as.factor(Diet),data = data)
res<-anova2$residuals
shapiro.test(x = res)
hist(res,main="Histogram of residuals",xlab="Residuals")


summary(anova2)

#FOr post hoc tests and analysis 
#we now perform Tukey HSD test to perform pairwise comparison of groups for determining which groups differ significantly 
TukeyHSD(anova2,ordered = TRUE)
