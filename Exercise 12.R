# 1.

Location_A <- c(81.6, 81.3, 82, 79.6, 78.4, 81.8, 80.2, 80.7)

Location_B <- c(81.8, 84.7, 82, 85.6, 79.9, 83.2, 84.1, 85)

Location_C <- c(82.1, 79.6, 83.1, 80.7, 81.8, 79.9, 82.6, 81.9)

data <- c(Location_A, Location_B, Location_C)

l <- rep(c("A", "B", "C"), c(8, 8, 8))

location <- factor(l)

# brix_degrees <- data.frame(data, location)

brix_degrees_ <- data.frame(data[-13], location[-13])

# attach(brix_degrees)
attach(brix_degrees_)

# brix_degrees.aov <- aov(data ~ location, data = brix_degrees)
# summary(brix_degrees.aov)

names(brix_degrees_)<-c("new.data","new.location")

brix_degrees.aov_ <- aov(new.data ~ new.location, data = brix_degrees_)
summary(brix_degrees.aov_)

# i. 

# SHatSquared <- var(brix_degrees$data)

est.stdev <- sqrt(1.759 * (1-(1/3))) 
std.res <- residuals(brix_degrees.aov_)/est.stdev
plot(fitted(brix_degrees.aov_), std.res)

# ii. 

library(MASS)
plot(fitted(brix_degrees.aov), stdres(brix_degrees.aov))

library(MASS)
plot(fitted(brix_degrees.aov_), stdres(brix_degrees.aov_))

# In the plot it is apparent that the dispersion of the 
# standardized residuals increases with an increase of the
# fitted values.

# This would suggest that there is evidence to suggest that in 
# this case there is a tendency for the error variance to 
# increase with treatment mean.

# iii.

summary(stdres(brix_degrees.aov))

hist(stdres(brix_degrees.aov), breaks=seq(-2.5, 2, by = 0.5))
qqnorm(stdres(brix_degrees.aov))
qqline(stdres(brix_degrees.aov))

summary(stdres(brix_degrees.aov_))

hist(stdres(brix_degrees.aov_), breaks=seq(-2.5, 2, by = 0.5))
qqnorm(stdres(brix_degrees.aov_))
qqline(stdres(brix_degrees.aov_))

stdres(brix_degrees.aov_)

# The largest standardized residual in absolute value is 2.39.
# which is large and therefore could potentially be seen as an
# outlier.

# abs(min(stdres(brix_degrees.aov)))

# From the histogram it is apparent that the residuals are 
# negatively skewed. 

# From the QQ-Plot there is some departure from normality at the
# top of the line.

# iv

min(brix_degrees$data)

max(brix_degrees$data)

Location_A <- c(81.6, 81.3, 82, 79.6, 78.4, 81.8, 80.2, 80.7)

Location_B <- c(81.8, 84.7, 82, 85.6, 79.9, 83.2, 84.1, 85)

m <- mean(Location_B)

Location_B <- c(81.8, 84.7, 82, m, 79.9, 83.2, 84.1, 85)

Location_C <- c(82.1, 79.6, 83.1, 80.7, 81.8, 79.9, 82.6, 81.9)

data <- c(Location_A, Location_B, Location_C)

l <- rep(c("A", "B", "C"), c(8, 8, 8))

location <- factor(l)

brix_degrees <- data.frame(data, location)

attach(brix_degrees)

brix_degrees.aov <- aov(data ~ location, data = brix_degrees)
summary(brix_degrees.aov)

SHatSquared <- var(brix_degrees$data)

est.stdev <- sqrt(SHatSquared * (1-(1/3))) 
std.res <- residuals(brix_degrees.aov)/est.stdev
plot(fitted(brix_degrees.aov), std.res)

library(MASS)
plot(fitted(brix_degrees.aov), stdres(brix_degrees.aov))

summary(stdres(brix_degrees.aov))

hist(stdres(brix_degrees.aov), breaks=seq(-2.5, 2, by = 0.5))
qqnorm(stdres(brix_degrees.aov))
qqline(stdres(brix_degrees.aov))

# Removing the max data point of 85.6 does't seem to have much of an impact
# on the absolute value of the largest standardized residual which is now
# 2.33 in absolute value.

# 2.

library(MASS)

data <- c(7.1,6.1,6.9,5.6,6.4,
          6.7,5.1,5.9,5.1,5.8,
          7.1,5.8,6.2,5.0,6.2,
          6.7,5.4,5.7,5.2,5.3)            
          
d <- rep(c("A", "B", "C", "D"), 5)      
               
drug <- factor(d)

rabbit_experiment <- data.frame(data, drug)

rabbit_experiment.aov <- aov(data ~ drug, data = rabbit_experiment)
summary(rabbit_experiment.aov)

SHatSquared <- var(rabbit_experiment$data)

est.stdev <- sqrt(SHatSquared * (1-(1/4))) 
std.res <- residuals(rabbit_experiment.aov)/est.stdev
plot(fitted(rabbit_experiment.aov), std.res)

plot(fitted(rabbit_experiment.aov), stdres(rabbit_experiment.aov))

# From inspection of the plot, there is no 
# evidence to suggest in this case that there
# is any tendency for the error variance to
# to increase with treatment mean.

summary(stdres(rabbit_experiment.aov))

hist(stdres(rabbit_experiment.aov), breaks=seq(-2, 2, by = 0.5))
qqnorm(stdres(rabbit_experiment.aov))
qqline(stdres(rabbit_experiment.aov))          

# The largest standardized residual in absolute value is 1.61
# which is not extreme. Therefore, outliers are not an issue.

abs(min(stdres(brix_degrees.aov)))

# From the histogram it is apparent that the data is
# approximately normally distributed.

# From the QQ-Plot there is some mild departure from normality at the
# top and bottom of the line.            
        
    
    