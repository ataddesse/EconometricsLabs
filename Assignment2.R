library(AER)
# ======================================================================================================
# Chapter 5 exercise 2
# Loading data

dtaChapter5_2 <- get(load("Ch5_Exercise2_Baseball_attendence.RData"))
View(dtaChapter5_2)

#Identifying all the dependent and independent variables
home_attend = dtaChapter5_2$home_attend
wins= dtaChapter5_2$wins
runs_scored= dtaChapter5_2$runs_scored
runs_allowed= dtaChapter5_2$runs_allowed
season= dtaChapter5_2$season
#a
restricted_a <- lm(home_attend ~ wins + runs_allowed + runs_scored)
summary(restricted_a)
#c
restricted_b <- lm(home_attend ~ wins + runs_allowed + runs_scored + season)

summary(restricted_b)
#d
auxreg1 <- lm(runs_scored ~ season, data=dtaChapter5_2)
auxreg1
r <- summary(auxreg1)$r.squared
vif1 <- 1/(1-r)
plot(season, runs_scored)
abline(auxreg1)

#e
restricted_b


# ======================================================================================================
# Chapter 6 exercise 4
# Loading data

dtaChapter6_4 <- get(load("Ch6_Exercise4_Speeding_tickets.RData"))
View(dtaChapter6_4)
MPHover = dtaChapter6_4$MPHover
amount = dtaChapter6_4$Amount
age = dtaChapter6_4$Age
female = dtaChapter6_4$Female
black = dtaChapter6_4$Black
hispanic = dtaChapter6_4$Hispanic
statepol = dtaChapter6_4$StatePol
outtown = dtaChapter6_4$OutTown
outstate = dtaChapter6_4$OutState

ols_unrestiricted = lm(amount ~ MPHover + age + female + black + hispanic + statepol + outtown + outstate)
summary(ols_unrestiricted)

# a
ols_a = lm(amount ~ female) 
summary(ols_a)
b1_female <- summary(ols_a)$coefficients[2]
std_error = coef(summary(ols_a))["female", "Std. Error"]
t_female = b1_female/std_error
t_female
degfreedom = ols_a$df 
degfreedom
p1 = 2*(1-pt(abs(t_female), degfreedom))
p1
# b
ols_b = lm(amount ~ female + age + MPHover) 
summary(ols_b)
b2_female <- summary(ols_b)$coefficients[2]
std_error2 = coef(summary(ols_b))["female", "Std. Error"]
t_female2 = b2_female/std_error2
t_female2
p2 = 2*(1-pt(abs(t_female2), degfreedom))
p2

#c
x = black*hispanic
ols_c = lm(amount ~ black + hispanic) 
summary(ols_c)

#d
summary(ols_unrestiricted)

#e
ols_e = lm(amount ~ MPHover + female + black + hispanic)
summary(ols_e)
plot(jitter(MPHover), jitter(amount),
     xlab = "Miles per hour over speed limit", ylab = "Fine amount")

# Get the intercept term for each regression line
# MPHOver coefficient
cof_MPHover = coef(ols_e)[1]
# Female coefficient
cof_female = coef(ols_e)[2]
# Black coefficient
cof_black = coef(ols_e)[3]
# Hispanic coefficient
cof_hispanic = coef(ols_e)[4]

# add line for female and MPHover
abline(cof_female, cof_MPHover, col = 'red', lwd = 1) 
# add line for black and MPHover
abline(cof_black, cof_MPHover, col = 'orange', lwd = 1) 
# add line for hispanic and MPHover
abline(cof_hispanic, cof_MPHover, col = 'green', lwd = 1) 
library(zoom)
