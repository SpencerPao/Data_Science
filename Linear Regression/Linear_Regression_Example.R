# setwd('location\of\beer.csv')

beer <- read.csv("beer.csv")
str(beer)

attach(beer)

# Getting an idea on what the data looks like
plot(Carbohydrates, Calories)

# Correlation
cor.test(Carbohydrates, Calories)

# Normality
qqnorm(Carbohydrates, main = 'Normal QQ plot Carbs')
qqline(Carbohydrates)

qqnorm(log(Carbohydrates), main = 'Log Normal QQ plot Carbs')
qqline(log(Carbohydrates))

# Linear Model
mod <- lm(Calories ~ Carbohydrates, data = beer)

# Interpreting the model
# Null hypothesis B_x = 0
# Alt. Hyp B_x != 0
summary(mod)

anova(mod)

# Getting the Coefficients of the values
plot(Carbohydrates, Calories, main = "Carbs vs Calories")
abline(mod, col = 'red', lwd = 3)

confint(mod)

# Linear Model
# Carbs = 67.5254 + Carbohydrates * 7.2368

detach(beer)
