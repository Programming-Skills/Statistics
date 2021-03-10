# Exercise 8.

# 1.

Location_A <- c(81.6, 81.3, 82, 79.6, 78.4, 81.8, 80.2, 80.7)

Location_B <- c(81.8, 84.7, 82, 85.6, 79.9, 83.2, 84.1, 85)

Location_C <- c(82.1, 79.6, 83.1, 80.7, 81.8, 79.9, 82.6, 81.9)

data <- c(Location_A, Location_B, Location_C)

l <- rep(c("A", "B", "C"), c(8, 8, 8))

location <- factor(l)

brix_degrees <- data.frame(data, location)

attach(brix_degrees)

brix_degrees.aov <- aov(data ~ location, data = brix_degrees)
summary(brix_degrees.aov)

# cht <- glht(model = brix_degrees.aov)
# summary(cht, test = univariate())

# In this example the A-B pair differs at the 5% level. With 
# the small p-values we reject the null hypothesis that the
# treatment location B is equal to location A.

# fit: aov(formula = data ~ location, data = brix_degrees)

# Linear Hypotheses:
#        Estimate Std. Error t value Pr(>|t|)    
#        (Intercept) == 0  80.7000     0.5361 150.526  < 2e-16 ***
#        locationB == 0     2.5875     0.7582   3.413  0.00262 ** 
#        locationC == 0     0.7625     0.7582   1.006  0.32602    
---

# 2.

Soil_A <- c(12.8, 13.4, 11.2, 11.6, 9.4, 10.3, 14.1, 11.9, 10.5, 10.4)

Soil_B <- c(8.1, 10.3, 4.2, 7.8, 5.6, 8.1, 12.7, 6.8, 6.9, 6.4)

Soil_C <- c(9.8, 10.6, 9.1, 4.3, 11.2, 11.6, 8.3, 8.9, 9.2, 6.4)

Soil_D <- c(16.4, 8.2, 15.1, 10.4, 7.8, 9.2, 12.6, 11, 8, 9.8)

data <- c(Soil_A, Soil_B, Soil_C, Soil_D)

s <- rep(c("A", "B", "C", "D"), each=10)

soil <- factor(s)

moisture_content_percentage <- data.frame(data, soil)

attach(moisture_content_percentage)

moisture_content_percentage.aov <- aov(data ~ soil, data = moisture_content_percentage)
summary(moisture_content_percentage.aov)

# In this case the value of the F-statistic is 5.722 with p-value 0.00262, so we reject the null hypothesis
# at the 1% significance level.

# We deduce that there is very strong evidence indeed that the soil type influences the moisture content percentage.

m <- mean(Soil_A)
m

tt <- qt(0.975, 36)
tt

MSr <- var(Soil_A)
MSr

SE <- sqrt(MSr/10)
SE

CI <- c(m - tt * SE, m + tt * SE)
CI



# 3.

data <- c(20.935, 18.279, 28.535, 20.182,
          17.123, 14.815, 37.227, 34.340,
          29.590, 19.973, 30.529, 29.023,
          19.013, 21.200, 27.998, 18.792,
          15.919, 11.280, 38.853, 34.707,
          28.092, 20.096, 29.177, 28.176,
          20.332, 19.389, 30.073, 19.203,
          15.285, 12.153, 40.017, 36.307,
          28.304, 20.477, 30.795, 28.701)

# m <- matrix(data = data, nrow = 9, ncol = 4, byrow = TRUE)

# m.t <- t(m)

# Pos_1 <- m.t[1,]
# Pos_2 <- m.t[2,]
# Pos_3 <- m.t[3,]
# Pos_4 <- m.t[4,]

# data <- c(Pos_1, Pos_2, Pos_3, Pos_4)

# p <- rep(c("Pos_1", "Pos_2", "Pos_3", "Pos_4"), each=9)
p <- rep(c("Pos_1", "Pos_2", "Pos_3", "Pos_4"), 9)

position <- factor(p)

wear_of_tyres <- data.frame(data, position)

attach(wear_of_tyres)

by(data, position, summary)

plot(position, 
     data, 
     horizontal =TRUE,
     main="Wear of Tyres", 
     ylab = "Position", xlab = "Wear in mm", 
     col=4, cex.axis=1.5, cex.lab=1.5, cex.main=1.1)

# dev.off()

detach()

# Position 4 is approximately normally distributed, whereas, the other positions such as position 2 and 3 are highly positively and negatively skewed respectively. 

# Position 2 has the mimimum median wear at approximately 19mm, whereas, position 3 has the highest median wear at 31mm.

# Position 4 has the highest IQR 34-20 = 14mm (approximately).

wear_of_tyres.aov <- aov(data ~ position, data = wear_of_tyres)
summary(wear_of_tyres.aov)

# In this case the value of the F-statistic is 13.74 with p-value 6.28e-06, so we reject the null hypothesis
# at the 1% significance level.

# We deduce that there is very strong evidence indeed that the position has an effect on the wear of the tyre.
