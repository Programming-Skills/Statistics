# i. 
Initial_weight <- c(50, 64, 76, 64, 74, 60, 69, 68, 56, 48, 57, 59, 46, 45, 65)
Weight_gain <- c(128, 159, 158, 119, 133, 112, 96, 126, 132, 118, 107, 106, 82, 103, 104)

kitten.data <- data.frame(Initial_weight, Weight_gain)

rm(Initial_weight, Weight_gain)
attach(kitten.data)
plot(Initial_weight, Weight_gain)

# The plot shows weight gain increases seemingly at 
# random with no apparent linear relationship.

kitten.lm <- lm(Weight_gain ~ Initial_weight, data = kitten.data)
summary(kitten.lm)

# The output show that the coefficients of the fitted 
# regression line are β0 = 54.9502 and β1 = 1.0641.

# The p-value for both is insignificant, and therefore, it
# would appear there is no linear relationship.

anova(kitten.lm)

## Direct Calculation of the Observed CI.

# k1 is the upper 2.5% percentage point of the 
# t-distribution with 10 d.o.f. 
k1 <- qt(0.975, 13)

# k2 is the half-length of the interval
k2 <- k1 * 0.5259 

# k3 is the estimated value of the slope.
k3 <- 1.0641
CI <- c(k3 - k2, k3 + k2)
CI

# Thus the 95% confidence interval for β1 is (-0.07203788  2.20023788).

fitted.values <- fitted(kitten.lm)
fitted.values

leverages <- lm.influence(kitten.lm)$hat
library(MASS)
std.residuals <- stdres(kitten.lm)
diagnostics <- data.frame(leverages, std.residuals)
diagnostics

x <- data.frame(Initial_weight = 74)
predict.Weight_gain <- predict(kitten.lm, x, se.fit = TRUE)

predict.Weight_gain$fit

predict.Weight_gain$se.fit

predict.Weight_gain$df

predict.Weight_gain$residual.scale

library("EnvStats")

pointwise(predict.Weight_gain, 0.95)


# ii

Year <- c(1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980)
Population_density <- c(14.2, 17.8, 21.5, 26.0, 29.9, 34.7, 37.2, 42.6, 50.6, 57.5, 64.0)
Log_population_density <- log(Population_density)

Population.data <- data.frame(Year, Population_density)
Population.data

rm(Year, Population_density)
attach(Population.data)
plot(Year, Population_density)

# The plot shows that population density increases approximately
# linearly with year.

Population.lm <- lm(Population_density ~ Year, data = Population.data)
summary(Population.lm)

# The output shows that the coefficients of the fitted regression
# line are β0 = -903.73455 and β1 = 0.48691.

# The p-value for both is highly significant, and therefore, it
# would appear there is strong linear relationship.

anova(Population.lm)

## Direct Calculation of the Observed CI.

# k1 is the upper 2.5% percentage point of the 
# t-distribution with 10 d.o.f. 
k1 <- qt(0.975, 9)

# k2 is the half-length of the interval
k2 <- k1 * 0.02295

# k3 is the estimated value of the slope.
k3 <- 0.48691
CI <- c(k3 - k2, k3 + k2)
CI

# Thus the 95% confidence interval for β1 is (0.4349935, 0.5388265).

fitted.values <- fitted(Population.lm)
fitted.values

leverages <- lm.influence(Population.lm)$hat
library(MASS)
std.residuals <- stdres(Population.lm)
diagnostics <- data.frame(leverages, std.residuals)
diagnostics

x <- data.frame(Year = 1930)
predict.year <- predict(Population.lm, x, se.fit = TRUE)
predict.year$fit

predict.year$se.fit

predict.year$df

predict.year$residual.scale

library(EnvStats)
pointwise(predict.year, 0.95)


