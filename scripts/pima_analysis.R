#Installing necessary packages
install.packages("tidyverse")
install.packages("tidyr")
library(dplyr) 
library(ggplot2)
install.packages('corrplot')
library(corrplot)

# Import dataset
pima_dataset <- read.csv('diabetes.csv', header = TRUE)


## Question B
pima_dataset

# Discard pregnancies variable.
pima_dataset_no_preg <- pima_dataset[,!names(pima_dataset) %in% c('Pregnancies')]
head(pima_dataset_no_preg)

# Clean dataset by replacing 0 with NA values.
cleaned_pima_dataset_no_preg <- pima_dataset_no_preg %>% mutate_at(c('Glucose', 'BloodPressure','SkinThickness', 'Insulin','BMI',
                                                                     'DiabetesPedigreeFunction','Age'), ~ na_if(., 0))

## Question C: 
# Summarize variables
attach(cleaned_pima_dataset_no_preg)
summary(cleaned_pima_dataset_no_preg) 

#Boxplot for the dependent variable
par(mfrow = c(2, 4))
boxplot(Glucose[!is.na(Glucose)], horizontal = FALSE, lwd = 2, col = "grey", xlab = "Glucose Variable", ylab = "Count", notch = TRUE, border = "black", outpch = 25, outbg = "green", whiskcol = "blue",whisklty = 2, lty = 1)
                                  points(mean(Glucose[!is.na(Glucose)]), pch = 20, cex = 1.5, col = "yellow")

boxplot(BloodPressure[!is.na(BloodPressure)], horizontal = FALSE, lwd = 2, col = "grey", xlab = "BP Variable", ylab = "Count", notch = TRUE, border = "black", outpch = 25, outbg = "green", whiskcol = "blue",whisklty = 2, lty = 1)
                                  points(mean(BloodPressure[!is.na(BloodPressure)]), pch = 20, cex = 1.5, col = "yellow")

boxplot(SkinThickness[!is.na(SkinThickness)], horizontal = FALSE, lwd = 2, col = "grey", xlab = "Skin Thickness Variable", ylab = "Count", notch = TRUE, border = "black", outpch = 25, outbg = "green", whiskcol = "blue",whisklty = 2, lty = 1)
                                  points(mean(SkinThickness[!is.na(SkinThickness)]), pch = 20, cex = 1.5, col = "yellow")

boxplot(Insulin[!is.na(Insulin)], horizontal = FALSE, lwd = 2, col = "grey", xlab = "Insulin Variable", ylab = "Count", notch = TRUE, border = "black", outpch = 25, outbg = "green", whiskcol = "blue",whisklty = 2, lty = 1)
                                  points(mean(Insulin[!is.na(Insulin)]), pch = 20, cex = 1.5, col = "yellow")

boxplot(BMI[!is.na(BMI)], horizontal = FALSE, lwd = 2, col = "grey", xlab = "BMI Variable", ylab = "Count", notch = TRUE, border = "black", outpch = 25, outbg = "green", whiskcol = "blue",whisklty = 2, lty = 1)
                                  points(mean(BMI[!is.na(BMI)]), pch = 20, cex = 1.5, col = "yellow")

boxplot(DiabetesPedigreeFunction, horizontal = FALSE, lwd = 2, col = "grey", xlab = "DPF Variable", ylab = "Count", notch = TRUE, border = "black", outpch = 25, outbg = "green", whiskcol = "blue",whisklty = 2, lty = 1)
                                  points(mean(DiabetesPedigreeFunction), pch = 20, cex = 1.5, col = "yellow")

boxplot(Age, horizontal = FALSE, lwd = 2, col = "grey", xlab = "Age Variable", ylab = "Count", notch = TRUE, border = "black", outpch = 25, outbg = "green", whiskcol = "blue",whisklty = 2, lty = 1)
                                  points(mean(Age), pch = 20, cex = 1.5, col = "yellow")

# Create barplot for the independent variable,outcome.
outcome_variable <- table(Outcome)
barplot(outcome_variable, col = "grey", xlab = "Outcome Variable", ylab = "Count", title = "Outcome Distribution")

# Create histograms for each dependent variable.
install.packages("ggplot2")
library(ggplot2)
ggplot(cleaned_pima_dataset_no_preg, aes(x=Age)) + geom_histogram(binwidth = 5, fill = "purple", color = "black") +
                                                              ggtitle("Age Distribution") +  xlab("Age") + ylab("Count") 

ggplot(cleaned_pima_dataset_no_preg, aes(x=BMI)) + geom_histogram(binwidth = 1, fill = "green", color = "black") +
                                                              ggtitle("BMI Distribution") + xlab("BMI") + ylab("Count")

ggplot(cleaned_pima_dataset_no_preg, aes(x=Glucose)) + geom_histogram(binwidth = 10, fill = "yellow", color = "black") +
                                                              ggtitle("Glucose Distribution") + xlab("Glucose") + ylab("Count")

ggplot(cleaned_pima_dataset_no_preg, aes(x=BloodPressure)) + geom_histogram(binwidth = 5, fill = "blue", color = "black") +
                                                              ggtitle("Blood Pressure Distribution") + xlab("Blood Pressure") + ylab("Count")

ggplot(cleaned_pima_dataset_no_preg, aes(x=SkinThickness)) + geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
                                                              ggtitle("Skin Thickness Distribution") + xlab("Skin Thickness") + ylab("Count")

ggplot(cleaned_pima_dataset_no_preg, aes(x=Insulin)) +  geom_histogram(binwidth = 20, fill = "brown", color = "black") +
                                                              ggtitle("Insulin Distribution") + xlab("Insulin") + ylab("Count")
  
ggplot(cleaned_pima_dataset_no_preg, aes(x=DiabetesPedigreeFunction)) + geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
                                                              ggtitle("Diabetes Pedigree Function Distribution") + xlab("Diabetes Pedigree Function") + ylab("Count")
detach(cleaned_pima_dataset_no_preg)


## Question D
attach(cleaned_pima_dataset_no_preg)

# Separating dataset on basis of the outcome variables 
pima_diabetic_dataset <- cleaned_pima_dataset_no_preg[which(Outcome ==1),] # diabetic 
pima_nondiabetic_dataset <-cleaned_pima_dataset_no_preg[which(Outcome ==0),] #nondiabetic
detach(cleaned_pima_dataset_no_preg)

# Perform normality test on each group of each predictor variable - Kolmogorov-Smirnov test
#1.Glucose
ks.test(pima_diabetic_dataset$Glucose, "pnorm", mean = mean(pima_diabetic_dataset$Glucose, na.rm = TRUE), sd = sd(pima_diabetic_dataset$Glucose, na.rm = TRUE))
ks.test(pima_nondiabetic_dataset$Glucose, "pnorm", mean = mean(pima_nondiabetic_dataset$Glucose, na.rm = TRUE), sd = sd(pima_nondiabetic_dataset$Glucose, na.rm = TRUE))

par(mfrow = c(2,4))
hist(pima_diabetic_dataset$Glucose,freq = FALSE,xlab = 'Glucose', main = "Diabetic", col = "blue")
hist(pima_nondiabetic_dataset$Glucose,freq = FALSE,xlab = 'Glucose', main = 'non-Diabetic', col = "light blue")

#2.Blood Pressure
ks.test(pima_diabetic_dataset$BloodPressure, "pnorm", mean = mean(pima_diabetic_dataset$BloodPressure, na.rm = TRUE), sd = sd(pima_diabetic_dataset$BloodPressure, na.rm = TRUE))
ks.test(pima_nondiabetic_dataset$BloodPressure, "pnorm", mean = mean(pima_nondiabetic_dataset$BloodPressure, na.rm = TRUE), sd = sd(pima_nondiabetic_dataset$BloodPressure, na.rm = TRUE))

par(mfrow = c(2,4))
hist(pima_diabetic_dataset$BloodPressure,freq = FALSE,xlab = 'BP', main = "Diabetic", col = "blue")
hist(pima_nondiabetic_dataset$BloodPressure,freq = FALSE,xlab = 'BP', main = 'non-Diabetic', col ="light blue")

#3.Skin Thickness
ks.test(pima_diabetic_dataset$SkinThickness, "pnorm", mean = mean(pima_diabetic_dataset$SkinThickness, na.rm = TRUE), sd = sd(pima_diabetic_dataset$SkinThickness, na.rm = TRUE))
ks.test(pima_nondiabetic_dataset$SkinThickness, "pnorm", mean = mean(pima_nondiabetic_dataset$SkinThickness, na.rm = TRUE), sd = sd(pima_nondiabetic_dataset$SkinThickness, na.rm = TRUE))

par(mfrow = c(2,4))
hist(pima_diabetic_dataset$SkinThickness,freq = FALSE,xlab = 'Skin Thickness', main = "Diabetic", col = "blue")
hist(pima_nondiabetic_dataset$SkinThickness,freq = FALSE,xlab = 'Skin Thickness', main = 'non-Diabetic', col = "light blue")

#4.Insulin
ks.test(pima_diabetic_dataset$Insulin, "pnorm", mean = mean(pima_diabetic_dataset$Insulin, na.rm = TRUE), sd = sd(pima_diabetic_dataset$Insulin, na.rm = TRUE))
ks.test(pima_nondiabetic_dataset$Insulin, "pnorm", mean = mean(pima_nondiabetic_dataset$Insulin, na.rm = TRUE), sd = sd(pima_nondiabetic_dataset$Insulin, na.rm = TRUE))

par(mfrow = c(2,4))
hist(pima_diabetic_dataset$Insulin,freq = FALSE,xlab = 'Insulin', main = "Diabetic", col = "blue")
hist(pima_nondiabetic_dataset$Insulin,freq = FALSE,xlab = 'Insulin', main = 'non-Diabetic', col = "light blue")

#5. BMI
ks.test(pima_diabetic_dataset$BMI, "pnorm", mean = mean(pima_diabetic_dataset$BMI, na.rm = TRUE), sd = sd(pima_diabetic_dataset$BMI, na.rm = TRUE))
ks.test(pima_nondiabetic_dataset$BMI, "pnorm", mean = mean(pima_nondiabetic_dataset$BMI, na.rm = TRUE), sd = sd(pima_nondiabetic_dataset$BMI, na.rm = TRUE))

par(mfrow = c(2,4))
hist(pima_diabetic_dataset$BMI,freq = FALSE,xlab = 'BMI', main = "Diabetic", col = "blue")
hist(pima_nondiabetic_dataset$BMI,freq = FALSE,xlab = 'BMI', main = 'non-Diabetic', col = "light blue")

#6. Diabetic Pedigree Function
ks.test(pima_diabetic_dataset$DiabetesPedigreeFunction, "pnorm", mean = mean(pima_diabetic_dataset$DiabetesPedigreeFunction, na.rm = TRUE), sd = sd(pima_diabetic_dataset$DiabetesPedigreeFunction, na.rm = TRUE))
ks.test(pima_nondiabetic_dataset$DiabetesPedigreeFunction, "pnorm", mean = mean(pima_nondiabetic_dataset$DiabetesPedigreeFunction, na.rm = TRUE), sd = sd(pima_nondiabetic_dataset$DiabetesPedigreeFunction, na.rm = TRUE))

par(mfrow = c(2,4))
hist(pima_diabetic_dataset$DiabetesPedigreeFunction,freq = FALSE,xlab = 'Diabetic Pedigree Function', main = "Diabetic", col = "blue")
hist(pima_nondiabetic_dataset$DiabetesPedigreeFunction,freq = FALSE,xlab = 'Diabetic Pedigree Function', main = 'non-Diabetic', col = "light blue")

#7. Age
ks.test(pima_diabetic_dataset$Age, "pnorm", mean = mean(pima_diabetic_dataset$Age, na.rm = TRUE), sd = sd(pima_diabetic_dataset$Age, na.rm = TRUE))
ks.test(pima_nondiabetic_dataset$Age, "pnorm", mean = mean(pima_nondiabetic_dataset$Age, na.rm = TRUE), sd = sd(pima_nondiabetic_dataset$Age, na.rm = TRUE))

par(mfrow = c(1,2))
hist(pima_diabetic_dataset$Age,freq = FALSE,xlab = 'Age', main = "Diabetic", col = "blue")
hist(pima_nondiabetic_dataset$Age,freq = FALSE,xlab = 'Age', main = 'non-Diabetic', col = "light blue")

#Hypothesis Testing of Variables
t.test(pima_diabetic_dataset$BloodPressure, pima_nondiabetic_dataset$BloodPressure, paired = FALSE)
t.test(pima_diabetic_dataset$SkinThickness, pima_nondiabetic_dataset$SkinThickness, paired = FALSE)
wilcox.test(pima_diabetic_dataset$Glucose, pima_nondiabetic_dataset$Glucose, alternative = "two.sided")
wilcox.test(pima_diabetic_dataset$Insulin, pima_nondiabetic_dataset$Insulin, alternative = "two.sided")
wilcox.test(pima_diabetic_dataset$BMI, pima_nondiabetic_dataset$BMI, alternative = "two.sided")
wilcox.test(pima_diabetic_dataset$DiabetesPedigreeFunction, pima_nondiabetic_dataset$DiabetesPedigreeFunction, alternative = "two.sided")
wilcox.test(pima_diabetic_dataset$Age, pima_nondiabetic_dataset$Age, alternative = "two.sided")

#Create Boxplots
library(RColorBrewer)
par(mfrow = c(2,4))
with(cleaned_pima_dataset_no_preg, boxplot(Glucose~Outcome, lwd = 2, col = brewer.pal(n = 3, name = 'RdBu'), names = c("Diabetic", "Non-Diabetic"), 
                               ylab = 'Count', main = "Glucose by Diabetes Outcome", notch = TRUE, border = "black", outpch = 23, outbg = "yellow", whiskcol = "red", whisklty = 2, lty = 1))
with(cleaned_pima_dataset_no_preg, boxplot(BloodPressure~Outcome, lwd = 2, col = brewer.pal(n = 3, name = 'RdBu'), names = c("Diabetic", "Non-Diabetic"), 
                               ylab = 'Count', main = "Blood Pressure by Diabetes Outcome", notch = TRUE, border = "black", outpch = 23, outbg = "yellow", whiskcol = "red", whisklty = 2, lty = 1))
with(cleaned_pima_dataset_no_preg, boxplot(SkinThickness~Outcome, lwd = 2, col = brewer.pal(n = 3, name = 'RdBu'), names = c("Diabetic", "Non-Diabetic"), 
                               ylab = 'Count', main = "Skin Thickness by Diabetes Outcome", notch = TRUE, border = "black", outpch = 23, outbg = "yellow", whiskcol = "red", whisklty = 2, lty = 1))
with(cleaned_pima_dataset_no_preg, boxplot(Insulin~Outcome, lwd = 2, col = brewer.pal(n = 3, name = 'RdBu'), names = c("Diabetic", "Non-Diabetic"), 
                               ylab = 'Count', main = "Insulin by Diabetes Outcome", notch = TRUE, border = "black", outpch = 23, outbg = "yellow", whiskcol = "red", whisklty = 2, lty = 1))
with(cleaned_pima_dataset_no_preg, boxplot(BMI~Outcome, lwd = 2, col = brewer.pal(n = 3, name = 'RdBu'), names = c("Diabetic", "Non-Diabetic"), 
                               ylab = 'Count', main = "BMI by Diabetes Outcome", notch = TRUE, border = "black", outpch = 23, outbg = "yellow", whiskcol = "red", whisklty = 2, lty = 1))
with(cleaned_pima_dataset_no_preg, boxplot(DiabetesPedigreeFunction~Outcome, lwd = 2, col = brewer.pal(n = 3, name = 'RdBu'), names = c("Diabetic", "Non-Diabetic"), 
                               ylab = 'Count', main = "Diabetes Pedigree Function by Diabetes Outcome", notch = TRUE, border = "black", outpch = 23, outbg = "yellow", whiskcol = "red", whisklty = 2, lty = 1))
with(cleaned_pima_dataset_no_preg, boxplot(Age~Outcome, lwd = 2, col = brewer.pal(n = 3, name = 'RdBu'), names = c("Diabetic", "Non-Diabetic"), ylab = 'Count', 
                               main = "BP by Diabetes Outcome", notch = TRUE, border = "black", outpch = 23, outbg = "yellow", whiskcol = "red", whisklty = 2, lty = 1))

## Question E
attach(cleaned_pima_dataset_no_preg)

#Test for Normality of Predictor Variables: Kolmogorov-Smirnov test
ks.test(Glucose, "pnorm", mean = mean(Glucose, na.rm = TRUE), sd = sd(Glucose, na.rm = TRUE))
ks.test(BloodPressure, "pnorm", mean = mean(BloodPressure, na.rm = TRUE), sd = sd(BloodPressure, na.rm = TRUE))
ks.test(SkinThickness, "pnorm", mean = mean(SkinThickness, na.rm = TRUE), sd = sd(SkinThickness, na.rm = TRUE))
ks.test(Insulin, "pnorm", mean = mean(Insulin, na.rm = TRUE), sd = sd(Insulin, na.rm = TRUE))
ks.test(BMI, "pnorm", mean = mean(BMI, na.rm = TRUE), sd = sd(BMI, na.rm = TRUE))
ks.test(DiabetesPedigreeFunction, "pnorm", mean = mean(DiabetesPedigreeFunction, na.rm = TRUE), sd = sd(DiabetesPedigreeFunction, na.rm = TRUE))
ks.test(Age, "pnorm", mean = mean(Age, na.rm = TRUE), sd = sd(Age, na.rm = TRUE))

#Testing for association between two variables
library(Hmisc)
pima_pearson_corr <- rcorr(as.matrix(cleaned_pima_dataset_no_preg[1:7]), type = 'pearson') #correlation test for normally distributed data.
pima_pearson_corr
pima_pearson_corr$P
pima_pearson_corr$r
pima_spearman_corr <- rcorr(as.matrix(cleaned_pima_dataset_no_preg[1:7]), type = 'spearman') #Correlation test for non-normally distributed data
pima_spearman_corr
pima_spearman_corr$P
pima_spearman_corr$r

#Correlation Matrix
library(corrplot)
par(mfrow = c(1,1))
corrplot(cor(cleaned_pima_dataset_no_preg[1:7], use = 'pairwise.complete.obs'), order = 'AOE', col = COL2('PRGn', 5), method = 'number', main = "Correlation plot of the Predictor Variables")

#Scatter plot matrix
pairs(cleaned_pima_dataset_no_preg)
detach(cleaned_pima_dataset_no_preg)

## Question F
# Load necessary libraries
install.packages('MASS')
library(MASS)
install.packages('broom')
library(broom)

# Remove missing values
pima_data <- na.omit(cleaned_pima_dataset_no_preg)

# Perform logistic regression analysis
pima_log_model <- glm(Outcome ~ ., family = binomial(), data = pima_data)

# Examine the summary of the fitted model
summary(pima_log_model)
summary(pima_log_model)$coefficients

# Use stepwise model selection based on AIC to select the best model
step_pima_log_model  <- stepAIC(pima_log_model)

# Examine the summary of the selected model
summary(step_pima_log_model)

# Obtain the odds ratios for the selected model
odds_ratios <- tidy(step_pima_log_model, exponentiate = TRUE, conf.level = 0.95)
print(odds_ratios)

# Plot the estimated coefficients or odds ratios for each predictor variable
library(ggplot2)

# Bar plot of estimated coefficients
ggplot(data = tidy(pima_log_model)) +
  geom_bar(aes(x = term, y = estimate), stat = "identity") +
  coord_flip() +
  xlab("Predictor variable") +
  ylab("Estimated coefficient") +
  ggtitle("Estimated coefficients for each predictor variable in logistic regression model")

# `Bar plot of odds ratios
ggplot(data = odds_ratios) +
  geom_bar(aes(x = term, y = estimate), stat = "identity") +
  coord_flip() +
  xlab("Predictor variable") +
  ylab("Odds ratio") +
  ggtitle("Odds ratios for each predictor variable in logistic regression model")


## Question G
#Create  subgroups
lin_pima_glu <- subset(cleaned_pima_dataset_no_preg, is.na(Glucose))     #Creates subgroup with missing  glucose variable
lin_pima_no_age <- subset(cleaned_pima_dataset_no_preg, !is.na(Age))          #Creates subgroup with non-missing age variable


# Perform logistic regression analysis for Glucose using non-missing data
model_data <- lm(Glucose ~ Age, data = lin_pima_no_age)       

#print summary
summary(model_data)

#Create Plots: Residual vs Fitted
par(mfrow = c(2,2)) 
plot(model_data)
mtext("Regression Plots ", side = 3, line = - 2, outer = TRUE, cex = 1.5)

#Predict the missing entries for glucose
lin_pima_no_age$prediction_model <- predict(model_data, newdata = lin_pima_no_age)
glu_prediction <- round(na.omit(lin_pima_no_age$prediction_model), 0)
head(glu_prediction)
