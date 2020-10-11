# Kody Coppock
# 5/4/2020
# Lab Section 11
# Final Project
setwd("D://QTM 100 Datasets")
pres <- read.csv("pres_2016.csv", header = T)

### Data Cleaning ###
summary(pres$cvap)
## remove values coded as missing, no values too small to be illogical
pres$cvap[pres$cvap == 999999999] <- NA

pres$median_hh_inc[pres$median_hh_inc == 999999999] <- NA

## Remove missing values or percentages greater than 1
sum(pres$white_pct>100)
pres$white_pct[pres$white_pct > 100 | pres$white_pct == 999999999] <- NA

sum(pres$clf_unemploy_pct>100)
pres$clf_unemploy_pct[pres$clf_unemploy_pct > 100 | pres$clf_unemploy_pct == 999999999] <- NA

## Remove vote totals that cannot exist or are coded as missing ##
sum(pres$trump16 > pres$total_votes)
# 10 true
pres$trump16[pres$trump16 > pres$total_votes | pres$trump16 == 999999999] <- NA
sum(pres$trump16 > pres$total_votes)

sum(pres$clinton16 > pres$total_votes)
# 8 true
pres$clinton16[pres$clinton16 > pres$total_votes | pres$clinton16 == 999999999] <- NA

sum(pres$otherpres16 > pres$total_votes)
# 6 true
pres$otherpres16[pres$otherpres16 > pres$total_votes | pres$otherpres16 == 999999999] <- NA

sum(pres$total_votes > pres$cvap)
# 9 true
pres$total_votes[pres$total_votes > pres$cvap | pres$total_votes == 999999999] <- NA

# drop NAs
pres_clean <- na.omit(pres)
##

### ###

### Response Variables ###
# dichotomize trumps votes into whether or not he won a majority in that county
pres_clean$trumpmaj <- pres_clean$trump16>(pres_clean$total_votes/2)
pres_clean$trumpmaj

pres_clean$otherpres16
### ###

### Explanatory Variables ###
pres_clean$clf_unemploy_pct

pres_clean$maj_white <- (pres_clean$white_pct>50)
pres_clean$maj_white 

levels(pres_clean$median_hh_inc)
pres_clean$median_hh_inc <- cut(pres_clean$median_hh_inc, breaks = 5, labels = c("< 25k", "25k - 50k", "50k - 75k", "75k - 100k", "> 100k"))
### ###

### Tests ###
mean(pres_clean$clf_unemploy_pct)
sd(pres_clean$clf_unemploy_pct)
by(data = pres_clean$clf_unemploy_pct, INDICES = pres_clean$trumpmaj, FUN = mean)
by(data = pres_clean$clf_unemploy_pct, INDICES = pres_clean$trumpmaj, FUN = sd)

mean(pres_clean$maj_white)

dich_table_inc <- table(pres_clean$trumpmaj, pres_clean$median_hh_inc)
dich_table_inc
prop.table(dich_table_inc)

dich_table_tot <- table(pres_clean$median_hh_inc)
dich_table_tot
prop.table(dich_table_tot)

dich_table <- table(pres_clean$trumpmaj, pres_clean$maj_white)
dich_table
prop.table(dich_table)
chisq.test(dich_table)

res.aov <- aov(otherpres16 ~ median_hh_inc, data = pres_clean)
summary(res.aov)

regress <- lm(otherpres16 ~ clf_unemploy_pct, data=pres_clean)
summary(regress)
confint(regress)
### ###

### Plots ###
plot(pres_clean$otherpres16~pres_clean$clf_unemploy_pct,
     xlab= "Unemployment Percent",
     ylab= "Other President Votes",
     type="p")
barplot(prop.table(table(pres_clean$trumpmaj, pres_clean$maj_white)),
        xlab="Majority White",
        ylab="Proportion of Trump Majority")
boxplot(pres_clean$otherpres16~pres_clean$median_hh_inc,
        xlab= "Median Income Level",
        ylab= "Other President Votes",
        outline=F)
        