# Getting the iris Dataframe in R
df = iris

# Converting the Species identifier into a factor 
# because our independent variable must be categorical
df$Species = factor(as.numeric(df$Species))
is.factor(df$Species)


## Checking Assumptions

# Normality
hist(df$Sepal.Length)

# Boxplot
boxplot(df$Sepal.Length~df$Species,df)

# Linearity between covariate and Response variable
plot(df$Sepal.Length, df$petal.Length)


# Homogeneity of Regression slopes
lines(df, col = "2", lwd = 3, lty = 2)

#  For sepal length
abline(lm(df$Petal.Length ~ df$Sepal.Length), col = "orange", lwd = 3)

# For sepal width
lines(lowess(df$Petal.Length ~ df$Sepal.Width), col = "blue", lwd = 3)

# For petal width
lines(lowess(df$Petal.Length ~ df$Sepal.Width), col = "grey", lwd = 3)

# Legend
legend("bottom", legend = c("sepal_lenght", "Sepal_width", "Petal_width"),
       lwd = 3, lty = c(2, 1, 1), col = c("orange", "blue", "grey"))


# Running the Anova
fit = aov(Petal.Length ~ Species, df) 
summary(fit)

# ANCOVA Model
fit2=aov(Sepal.Width~Species+Sepal.Length,df)
Anova(fit2, type="III")
