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

cht <- glht(model = brix_degrees.aov, linfct = mcp(location = "Tukey"))
summary(cht, test = univariate())

# In this example the A-B pair differs at the 5% level. With 
# the small p-values we reject the null hypothesis that the
# treatment location B is equal to location A.

# fit: aov(formula = data ~ location, data = brix_degrees)

# Linear Hypotheses:
#        Estimate Std. Error t value Pr(>|t|)    
#        (Intercept) == 0  80.7000     0.5361 150.526  < 2e-16 ***
#        locationB == 0     2.5875     0.7582   3.413  0.00262 ** 
#        locationC == 0     0.7625     0.7582   1.006  0.32602    

# 2. 

# i. 

Soil_A <- c(12.8, 13.4, 11.2, 11.6, 9.4, 10.3, 14.1, 11.9, 10.5, 10.4)

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

# ii.

Soil_A <- c(12.8, 13.4, 11.2, 11.6, 9.4, 10.3, 14.1, 11.9, 10.5, 10.4)

Soil_B <- c(8.1, 10.3, 4.2, 7.8, 5.6, 8.1, 12.7, 6.8, 6.9, 6.4)

Abar_minus_Bbar <- mean(Soil_A) - mean(Soil_B) 
Abar_minus_Bbar

SED <- sqrt((2 * MSr)/10)
SED

tt <- qt(0.975, 36)
tt

lsd <- tt * SED
lsd

CI <- c(Abar_minus_Bbar - lsd, Abar_minus_Bbar + lsd) 
CI

confint(cht, calpha = univariate_calpha())

# 3. 

# i. 

t_5 <- qt(0.95, 22)
t_5

t_025 <- qt(0.975, 22)
t_025

t_1 <- qt(0.999, 22)
t_1

t_05 <- qt(0.9995, 22)
t_05

Tobs <- 2.57
Tobs

# ii.

pt_1 <- pt(Tobs, 22-1)
pt_1

pt_2 <- 2*pt(abs(Tobs), 22-1)
pt_2

# iii.

# Both p-values fail to reject at the 5% significance level.
