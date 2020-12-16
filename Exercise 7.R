# Exercise 7

# 1.

Location_A <- c(81.6, 81.3, 82, 79.6, 78.4, 81.8, 80.2, 80.7)

Location_B <- c(81.8, 84.7, 82, 85.6, 79.9, 83.2, 84.1, 85)

Location_C <- c(82.1, 79.6, 83.1, 80.7, 81.8, 79.9, 82.6, 81.9)

# hist(Location_C, breaks = 10)

data <- c(Location_A, Location_B, Location_C)

l <- rep(c("A", "B", "C"), c(8, 8, 8))

location <- factor(l)

brix_degrees <- data.frame(data, location)

attach(brix_degrees)

by(data, location, summary)

plot(location, 
     data, 
     horizontal= TRUE, 
     main="Brix Degrees in Various Colombian Locations", 
     ylab = "Location", xlab = "Brix Degrees", 
     col=4, cex.axis=1.5, cex.lab=1.5, cex.main=1.1)

# dev.off()

detach()

# The data for location C is negatively skewed, whereas the data for the 
# other locations is approximately normally distributed.

# Location B has the highest median brix degrees value of approximately 83.6.

# Location A has an interquartile range of approximately 1.5 brix degrees.

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

by(data, soil, summary)

plot(soil, 
     data, 
     horizontal =TRUE,
     main="Moisture Content % of Various Types of Soil", 
     ylab = "Location", xlab = "Moisture Content %", 
     col=4, cex.axis=1.5, cex.lab=1.5, cex.main=1.1)

# dev.off()

detach()

# Soil B is approximately normally distributed, whereas the other soil types 
# appear to be postively skewed.

# Soil B contains an outlier at the 12.7 moisture percentage mark.

# Soil A has the hightest median moisture content at 11.5%.

