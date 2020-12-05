# Chapter 3 exercise 3
# Loading data
dta <- get(load("Ch3_Exercise3_Height_and_Wages_UK.RData"))
x = dta$height33
y= dta$gwage33


#a
summary(lm(y ~ x))

#b
plot(x, y, xlab="Height at age 33", ylab="Wage at age 33")
abline(lm(y ~ x))

#c
install.packages("dplyr")
library(dplyr)
# Filter data based on the requirements
dta_filtered <- filter(dta, dta$height33 >= 40 & dta$gwage33 <= 400)
# Assign x and y values for the filtered data
x_filtered = dta_filtered$height33
y_filtered= dta_filtered$gwage33
# Model the data
lm(x_filtered ~ y_filtered)
summary(lm(x_filtered ~ y_filtered))
# Plot it
plot(x_filtered, y_filtered, xlab="Height at age 33", ylab="Wage at age 33")
abline(lm(x_filtered ~ y_filtered))

#d
dta <- get(load("Ch3_Exercise3_Height_and_Wages_UK.RData"))
#delete first highest wage value
dta<-subset(dta, dta$gwage33 != max(dta$gwage33))
#delete second highest wage value
dta<-subset(dta, dta$gwage33 != max(dta$gwage33))
#delete third highest wage value
dta<-subset(dta, dta$gwage33 != max(dta$gwage33))
#delete fourth highest wage value
dta<-subset(dta, dta$gwage33 != max(dta$gwage33))

# Filter out heights less than 40
dta_less_outliers_filtered <- filter(dta, dta$height33 >= 40)
# Assign x and y values
x_d = dta_less_outliers_filtered$height33
y_d = dta_less_outliers_filtered$gwage33

#Plot
plot(x_d, y_d, xlab="Height at age 33", ylab="Wage at age 33")
summary(lm(y_d ~ x_d))
abline(lm(x_d ~ y_d))

#e
dta_small <- head(dta_less_outliers_filtered, 800)
x_small = dta_small$height33
y_small= dta_small$gwage33
summary(lm(y_small ~ x_small))
plot(x_filtered, y_filtered, xlab="Height at age 33", ylab="Wage at age 33")

# Chapter 3 exercise 4
dta_4women <- get(load("Ch3_Exercise4_Divorce_rates_Women.RData"))
dta_4men <- get(load("Ch3_Exercise4_Divorce_rates_Men.RData"))
#a
x_4women = dta_4women$divorcerate
y_4women = dta_4women$hours
plot(x_4women, y_4women, main="Women", xlab="Hours", ylab="Divorce")

x_4men = dta_4men$divorcerate
y_4men = dta_4men$hours
plot(x_4men, y_4men, main="Men", xlab="Hours", ylab="Divorce")
#b
summary(lm(y_4women ~ x_4women))
summary(lm(y_4men ~ x_4men))
#c
dta_4men_germany <- filter(dta_4men, dta_4men$country == "Germany")
dta_4men_germany <- dta_4men_germany[complete.cases(dta_4men_germany)]
reg_men <- lm(dta_4men_germany$hours ~ dta_4men_germany$divorcerate)
# Calculated fitted value
fitted.values(reg_men)

plot(dta_4men_germany$hours, dta_4men_germany$divorcerate)
#d
dta_4women_spain <- filter(dta_4women, dta_4women$country == "Spain")
dta_4women_spain <- dta_4women_spain[complete.cases(dta_4women_spain)]
reg_women <- lm(dta_4women_spain$hours ~ dta_4women_spain$divorcerate)
# Calculated fitted value
fitted.values(reg_women)

plot(dta_4women_spain$hours, dta_4women_spain$divorcerate)

# Chapter 4 exercise 3
dta_ch4 <- get(load("Ch4_Exercise3_Presidents_and_Economy.RData"))
head(dta_ch4, 100)
# a
lm( dta_ch4$Unemployment ~ dta_ch4$LagDemPresident)
# b
lm(dta_ch4$ChangeGDPpc ~ dta_ch4$LagDemPresident)
# c
summary(lm(dta_ch4$Unemployment ~ dta_ch4$LagDemPresident))
summary(lm(dta_ch4$ChangeGDPpc ~ dta_ch4$LagDemPresident))

# d

# e
BetaRange = seq(0, 800, 4)
std_error = coef(summary(lm(dta_ch4$ChangeGDPpc ~ dta_ch4$LagDemPresident)))["dta_ch4$LagDemPresident", "Std. Error"]
PowerCurve = pnorm(BetaRange/std_error - 2.32)
plot(BetaRange, PowerCurve, xlab="Beta", ylab="Probability reject null", type="l")
