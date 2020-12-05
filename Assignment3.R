install.packages("plm")
dta <- get(load("Ch8_Exercise4_HOPE_scholarship.rdata"))
View(dta)

#a

dd1 = lm(dta$InCollege ~ dta$Georgia + dta$After  + dta$AfterGeorgia)
summary(dd1)

lm(dta$InCollege ~ dta$AfterGeorgia + factor(Unit) + factor(Year) + X2)

#b
# i
mean(dta$InCollege[dta$Georgia == 0 & dta$After == 0])


# ii
mean(dta$InCollege[dta$Georgia == 1 & dta$After == 0])

# iii
mean(dta$InCollege[dta$Georgia == 0 & dta$After == 1])

# iv
mean(dta$InCollege[dta$Georgia == 1 & dta$After == 1])

#c
plot(dta$After, dta$InCollege, type="n", xlab = "After 1992", ylab = "InCollege", main="Blue = Georgia groups, Green = Non Georgia groups",)
abline(lm(dta$InCollege[dta$Georgia==0] ~ dta$After[dta$Georgia==0]), col = "Blue")
abline(lm(dta$InCollege[dta$Georgia==1] ~ dta$After[dta$Georgia==1]), col = "Green")

#d
lm(dta$InColleg ~ dta$AfterGeorgia + factor(dta$StateCode) + factor(dta$Year))

#e
lm(dta$InColleg ~ dta$AfterGeorgia + factor(dta$StateCode) + factor(dta$Year) + dta$Age18)

#f

lowIncome = subset(dta, LowIncome == "1")
highIncome = subset(dta, LowIncome == "0")

dd2 = lm(lowIncome$InCollege ~ lowIncome$Georgia + lowIncome$After  + lowIncome$AfterGeorgia)
summary(dd2)

dd3 = lm(highIncome$InCollege ~ highIncome$Georgia + highIncome$After  + highIncome$AfterGeorgia)
summary(dd3)


t_r1 = -5.844/1.972
t_r1
p1 = 2*(1-pt(abs(t_r1), 49))
p1
