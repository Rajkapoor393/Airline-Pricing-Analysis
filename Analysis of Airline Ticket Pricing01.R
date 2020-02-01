# Analysis of Airline Ticket Pricing
# NAME: RAJ KAPOOR GUPTA
# EMAIL: mr.rajkapoor393@gmail.com
# COLLEGE : NIT ROURKELA


# Importing the dataset
sixAirlines <- read.csv(paste("SixAirlinesDataV2.csv", sep=""))
View(sixAirlines)
library(psych)
summary(sixAirlines)
library(corrplot)

library(corrgram)
str(sixAirlines)
boxplot(sixAirlines$SeatsEconomy, xlab= "Seats Economy", 
        ylab = "Seats Economy", main= "Seats Economy distribution", horizontal = TRUE)
boxplot(sixAirlines$SeatsPremium, xlab= "Seats Premium", ylab = "Seats Premium", 
        main= "Seats Premium distribution", horizontal = TRUE)
boxplot(sixAirlines$PitchEconomy, xlab= "Pitch Economy", ylab = "Pitch Economy",
        main= "Pitch Economy distribution", horizontal = TRUE)
boxplot(sixAirlines$PitchPremium, xlab= "PitchPremium", ylab = "PitchPremium", 
        main= "PitchPremium distribution", horizontal = TRUE)
boxplot(sixAirlines$WidthEconomy, xlab= "WidthEconomy", ylab = "WidthEconomy", 
        main= "WidthEconomy distribution", horizontal = TRUE)
boxplot(sixAirlines$WidthPremium, xlab= "WidthPremium", ylab = "WidthPremium",
        main= "WidthPremium distribution", horizontal = TRUE)
boxplot(sixAirlines$WidthPremium, xlab= "WidthPremium", ylab = "WidthPremium",
        main= "WidthPremium distribution", horizontal = TRUE)
boxplot(sixAirlines$PricePremium , xlab= "PricePremium ", ylab = "PricePremium ",
        main= "PricePremium  distribution", horizontal = TRUE)
boxplot(sixAirlines$PriceRelative, xlab= "PriceRelative", ylab = "PriceRelative", 
        main= "PriceRelative distribution", horizontal = TRUE)
boxplot(sixAirlines$SeatsTotal, xlab= "SeatsTotal", ylab = "SeatsTotal",
        main= "SeatsTotal distribution", horizontal = TRUE)
boxplot(sixAirlines$PitchDifference, xlab= "PitchDifference", ylab = "PitchDifference", 
        main= "PitchDifference distribution", horizontal = TRUE)
boxplot(sixAirlines$WidthDifference, xlab= "WidthDifference", ylab = "WidthDifference",
        main= "WidthDifference distribution", horizontal = TRUE)
boxplot(sixAirlines$PercentPremiumSeats, xlab= "PercentPremiumSeats", ylab = "PercentPremiumSeats", 
        main= "PercentPremiumSeats distribution", horizontal = TRUE)
plot(x=sixAirlines$SeatsEconomy,y=sixAirlines$PitchEconomy, xlab= "Seats Economy", 
     ylab = "Pitch Economy")
plot(x=sixAirlines$SeatsPremium,y=sixAirlines$PitchPremium, xlab= "Seats Premium",
     ylab = "Pitch Premium")
plot(x=sixAirlines$SeatsEconomy,y=sixAirlines$WidthEconomy, xlab= "Seats Economy", 
     ylab = "Width Economy")
plot(x=sixAirlines$SeatsPremium,y=sixAirlines$WidthPremium, xlab= "Seats Premium", 
     ylab = "Width Premmium")
plot(x=sixAirlines$PitchEconomy,y=sixAirlines$PriceEconomy, xlab= "Pitch Economy", 
     ylab = "Price Economy")
plot(x=sixAirlines$PitchPremium,y=sixAirlines$PricePremium, xlab= "Pitch Premium", ylab = "Price Premium")
plot(x=sixAirlines$WidthEconomy,y=sixAirlines$PriceEconomy, xlab= "Width Economy", ylab = "Price Economy")
plot(x=sixAirlines$WidthPremium,y=sixAirlines$PricePremium, xlab= "Width Premium", ylab = "Price Premium")
plot(x=sixAirlines$PitchDifference,y=sixAirlines$PriceRelative, xlab= "Pitch Difference", ylab = "Price Relative")

plot(x=sixAirlines$WidthDifference,y=sixAirlines$PriceRelative, xlab= "Width Difference", ylab = "Price Relative")

plot(x=sixAirlines$PercentPremiumSeats,y=sixAirlines$PriceRelative, xlab= "Percent premium Seats", ylab = "Price Relative")

library(corrplot)
library(corrgram)
mytable <- sixAirlines[,6:18]
cov(mytable)

cor(mytable)

corr.test(mytable, use= "complete")

corrgram(mytable, order= TRUE, lower.panel = panel.shade, upper.panel=panel.pie, text.panel = panel.txt,main="Corrgram of sixAirlines intercorrelation")

t.test(PriceRelative ~ IsInternational , data= sixAirlines)

t.test(PriceEconomy ~ Aircraft, data= sixAirlines)

t.test(PricePremium ~ Aircraft, data= sixAirlines)

t.test(PriceRelative ~ Aircraft, data= sixAirlines)

model <- lm(PriceEconomy ~ SeatsEconomy + PitchEconomy + WidthEconomy, data = mytable)
summary(model)

model <- lm(PricePremium~ SeatsPremium + PitchPremium + WidthPremium, data = mytable)
summary(model)

model <- lm(PriceRelative ~ PitchDifference + WidthDifference, data= mytable)
summary(model)

model <- lm(PriceEconomy ~ . , data= mytable)
summary(model)

model <- lm(PricePremium ~ . , data= mytable)
summary(model)

model <- lm(PriceRelative ~ . , data= mytable)
summary(model)




