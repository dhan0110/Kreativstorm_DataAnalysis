# Load the packages

x<-c("plyr", "dplyr", "tidyr", "moments", "haven", "ppcor","ggplot2","hrbrthemes", "MASS", 'car')
lapply(x, require, character.only = TRUE)


# BIRTHWEIGHT ANALYSIS
# Load the data

df <- Birthweight

#scatterplot, add linear regresison line
ggplot(df, aes(mheight, Length)) +
  geom_point() +
  stat_smooth(method = lm)


#linear regression model (lm(dependent variable ~ independent variable, data))
model <- lm(Length ~ mheight, df)


#check autocorrelation
durbinWatsonTest(model)

#calculate residuals from the model
residuals <- resid(model)

#plot them against the model to assess homoscedasticity
plot(fitted(model), residuals)

# Same plot, with labels and line
plot(model$fitted.values, residuals(model), 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")


#install.packages("lmtest")
#library(lmtest)

# Perform the Breusch-Pagan test
bptest(model)
# here P-value = 0.8711 . Data is homoscedastic

#create Q-Q plot for residuals to assess normality of residuals
qqnorm(residuals)

#add a straight diagonal line to the plot
qqline(residuals) 

# Goodness of fit
summary(model)

#check homogeneity of variances
var.test(df$Headcirc ~ df$smoker)
# P-value of lavene's test = 0.3175

#syntax: leveneTest(y ~ group, data = df)
leveneTest(Headcirc ~ factor(smoker),df)
# p-value >0.05, variances are equal across groups.

# Comparing means of birthweight between smoker/non-smoker - independent means t-test
# 1. Check normality of data
shapiro.test(df$Birthweight[df$smoker==0])
shapiro.test(df$Birthweight[df$smoker==1])
# Data is normal
# 2 .Check homogeneity of variances
var.test(Birthweight~smoker,df)
# p-value for is >0.05. So there is homogeneity of variances
# 3. Independent means t-test
#independent samples t-test - with homogeneity of variances
t.test(Birthweight ~ smoker, df, var.equal = TRUE)
# p-value = 0.0427 < 0.05. We reject the null hypothesis. Statistically there is difference between the two groups.

# Comparing birthweight between over35/not over35 - independent means t-test
# 1. Check normality of data
shapiro.test(df$Birthweight[df$mage35==0])
shapiro.test(df$Birthweight[df$mage35==1])
# Data is normal
# 2 .Check homogeneity of variances
var.test(Birthweight~mage35,df)
# p-value for both > 0.05. So there is homogeneity of variances
# 3. Independent means t-test
#independent samples t-test - with homogeneity of variances
t.test(Birthweight ~  mage35, df, var.equal = TRUE)
# p-value = 0.4922 > 0.05. This means that we cannot reject the null hypothesis. Statistically there's no difference between the groups

# CHOLESTEROL DATA

df_new <- Cholesterol

# Effectiveness of diet after 8 weeks

# 1. create a new variable to capture the difference between the values
df_new$diff_B_8 <- df_new$Before - df_new$After8weeks
# 2. Check normality
shapiro.test(df_new$diff_B_8)
# Difference between the values is normal
# 3. Dependent means t-test
t.test(df_new$Before, df_new$After8weeks, paired = TRUE)
# p-value < 0.05. We reject the null hypothesis. Statistically there's difference between the two means.
mean(df_new$Before) 
mean(df_new$After8weeks)
# Value after 8 weeks is lower than value Before. Diet is effective.

# 2. Was the diet more effective in the first 4 weeks of use or the last 4 weeks of use
# 1. Create two new variables to capture the difference in the first 4 weeks and last 4 weeks
df_new$diff_B_4 <- df_new$Before - df_new$After4weeks
df_new$diff_4_8 <- df_new$After4weeks - df_new$After8weeks
# 2. Check normality of the difference between them
shapiro.test(df_new$diff_B_4-df_new$diff_4_8)
# Differences are normal
# 3. Dependent means t-test
t.test(df_new$diff_B_4, df_new$diff_4_8, paired = TRUE)
# p-value < 0.05. We reject the null hypothesis. Statistically there's a significant difference (0.5) between the two means.
mean(df_new$diff_4_8)
mean(df_new$diff_B_4)

# From the values, we can see that the diet was effective in the first 4 weeks

# 3. If the average cholesterol concentration in healthy adults is 3 mmol/L, 
#    would you consider your sample (N=18) significantly better or worse than average adult population

# We need to do the one-sample t-test.
# 1. Check normality
shapiro.test(df_new$Before)
# p>0.05. Data is normal
# 2. Do the t-test
t.test(df_new$Before, m = 3)
# p-value < 0.05. We reject the null hypothesis. Statistically there's a significant difference between the given and observed.
# Here mean = 6.4 which is >> 3. So average cholesterol concentration is worser in the given sample.