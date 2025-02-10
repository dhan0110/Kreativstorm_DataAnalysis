#Load packages

x<-c("plyr", "dplyr", "tidyr", "moments","haven")
lapply(x, require, character.only = TRUE)

# Load the data

df <- Birthweight

# Get basic descriptives of Birthweight, smoker columns

summary(df$Birthweight)

summary(df$smoker)

# Get mean birth weights, grouped by smoker column. 0 means non smoker. 1 means smoker.

tapply(df$Birthweight, df$smoker, mean)


# Get mean, maximum head circumference for babies of non smoking mothers

summary(df$Headcirc[df$smoker == 0])

# OR this for just mean alone

mean(subset(df, smoker == 0)$Headcirc)

# Get mean, minimum gestational age at birth for babies of smoking mothers

summary(df$Gestation[df$smoker==0])

# Get summary of pregnancy period based on smoker/non-smoker mother

tapply(df$Gestation, df$smoker, max)

# Get Summary of birthweight for smoking mothers

summary(df$Birthweight[df$smoker==1])

# To find the range of Birthweight for babies of smoking mothers

diff(range(df[df$smoker == 1, "Birthweight"]))

# To find the range of Birthweight for babies of non smoking mothers

diff(range(df[df$smoker == 0, "Birthweight"]))

#normality for variable "Head circumference"
shapiro.test(df$Headcirc[df$smoker==1])

# Z score of Headcirc = 35.05 in non-smoking mothers

mean_circ <- mean(subset(df, smoker == 0)$Headcirc)
std_circ <- sd(subset(df,smoker==0)$Headcirc)
X_circ <- 35.05

z_score <- (X_circ-mean_circ)/std_circ
print(z_score)

# Summary of birthweight for babies of  non smoking mothers
summary(df$Birthweight[df$smoker==0])

#normality for variable "birthweight" for babies of smoking mothers
shapiro.test(df$Birthweight[df$smoker==1])

#normality for variable 'birthweight'
shapiro.test(df$Birthweight)

# Z score of birthweight less than 4.2 for baby of a smoking mother
val<- 4.2
mean_bw <- mean(subset(df, smoker==1)$Birthweight)
std_bw <- sd(subset(df, smoker==1)$Birthweight)
z_scorebw <- (val-mean_bw)/std_bw
print(z_scorebw)
print(pnorm(z_scorebw))

# normality for variable length of baby of non-smoking mothers
shapiro.test(df$Length[df$smoker==0])

#standard score for the length of a baby of 48.5cm for non-smoking mothers
val_len <- 48.5
mean_len <- mean(subset(df, smoker==0)$Length)
std_len <- sd(subset(df, smoker==0)$Length)
z_scorelen <- (val_len-mean_len)/std_len
print(z_scorelen)

#probability that the length of baby for non-smoking mothers will be more than 55 cm
newlen <- 55
z_scorenew <- (newlen-mean_len)/std_len
print(z_scorenew)
print(1-pnorm(z_scorenew))
