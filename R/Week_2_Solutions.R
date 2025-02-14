# Load the packages

x<-c("plyr", "dplyr", "tidyr", "moments", "haven", "ppcor","ggplot2","hrbrthemes", "MASS")
lapply(x, require, character.only = TRUE)

df <- Birthweight

# Mean father's age

mean(df$fage)

# Mean father's age for low birth weight baby

mean(df$fage[df$lowbwt==1])

# Is the father's age normally distributed?

shapiro.test(df$fage)

# Apply the log transformation to the father's age.

df$log_fage <- log10(df$fage)
# what is the mean score of the transformed variable?
mean(df$log_fage)

# Is the new variable (log transform of father’s age) normally distributed? 
shapiro.test(df$log_fage)

# Is the variable “years father was in education” normally distributed? 
shapiro.test(df$fedyrs)

# Box-Code transformation of 'years father was in education'
x <- df$fedyrs
bc <- boxcox(lm(x ~ 1))
lambda <- bc$x[which.max(bc$y)]
x_transformed <- (x ^ lambda - 1) / lambda

# Mean of the new variable

mean(x_transformed)
# normality test for new variable
shapiro.test(x_transformed)

# Let's add it to the data frame as a new column
df$BC_fage <- x_transformed

# find the mean of bc_fage for mothers aged under 35
mean(df$BC_fage[df$mage<35])

# Correlation between father's age and birthweight

#Spearman's correlation test
cor.test(df$fage,df$Birthweight, method = "spearman")


# Point-biserial correlation - use pearson with 1st variable is continuous, second is binary
cor.test(df$Birthweight, df$smoker, method = 'pearson')

#Scatterplot
p2 <- ggplot(df, aes(x=df$Length, y=df$Birthweight)) +
  geom_point() +
  labs (x = "Length of baby", y = "Birth weight") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)


p2 <- ggplot(df, aes(x=df$Length, y=df$Birthweight)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  labs (x = "LoB", y = "Birth weight")
p2

# Relationship between length and birth weight
cor.test(df$Length, df$Birthweight, method='pearson')

# Partial correlation test between birth weight, length and head circumference of baby
pcor.test(df$Headcirc,df$Birthweight,df$Length, method = "pearson")


# Partial correlation test between birth weight, length and head circumference of baby
pcor.test(df$Length,df$Birthweight,df$Headcirc, method = "pearson")
