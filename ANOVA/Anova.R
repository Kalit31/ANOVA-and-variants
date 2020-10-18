# Install the required libraries
install.packages("readxl")
library(readxl)

# Read the dataset from excel 
Car_Insurance <- read_excel("/home/data/BITS/BITS 3-1/Applied Stastical Methods/ANOVA-and-variants/ANOVA/Car Insurance.xlsx")

# Carry out Shapiro Normality test
shapiro.test(Car_Insurance$Chicago)
shapiro.test(Car_Insurance$Houston)
shapiro.test(Car_Insurance$`New York`)
shapiro.test(Car_Insurance$Philadelphia)

# Concatenate cities
cities <- c(rep('Chicago',9),rep('Houston',9),
         rep('New York',9),rep('Philadelphia',9))

# Concatenate insurance prices
prices <- c(Car_Insurance$Chicago,Car_Insurance$Houston,
            Car_Insurance$`New York`,Car_Insurance$Philadelphia)

# Create a data frame ( cities vs prices )
df <- data.frame(cities,prices)

# Plots a box plot for prices at each city
plot(prices~cities,data = df)

# Carry out ANOVA analysis
insurance.aov <- aov(prices~cities,data=df)
summary(insurance.aov)


