install.packages("MASS")
install.packages("lmridge")
install.packages("robustbase")
install.packages("performance")
install.packages("orcutt")
install.packages("flexmix")
install.packages("sandwich")

library(dplyr)
library(readr)
library(nortest)
library(lmtest)
library(car)
library(flexmix)
library(sandwich)
library(MASS)
library(lmridge)
library(robustbase)
library(performance)
library(orcutt)

data <- read_csv("C:/Users/Stefan/Documents/BINUS/Materi/Semester 5/Regression Analysis/AoL/aol_dataset.csv")
head(data)

## Correlation Test (interpret)
cor.test(data$avg_length_school, data$human_development_index)
cor.test(data$life_expectancy, data$human_development_index)
cor.test(data$poverty_severity_index, data$human_development_index)
cor.test(data$protests, data$human_development_index)
cor.test(data$small_health_center, data$human_development_index)
cor.test(data$expected_year_school, data$human_development_index)
cor.test(data$construction_cost_index, data$human_development_index)
cor.test(data$health_index, data$human_development_index)
cor.test(data$general_doctors_count, data$human_development_index)
# Kita bisa cek ada hubungan linier ato enggak. Kalo nilai positif, negatif, atau mendekati 0 artinya apa.


## LM Model biasa
model <- lm(human_development_index ~ data$avg_length_school + data$life_expectancy + data$poverty_severity_index + data$protests + 
              data$small_health_center + data$expected_year_school + data$construction_cost_index + 
              data$health_index+ data$general_doctors_count, data=data)
summary(model)
AIC(model)
BIC(model)

## Residual Check
error <- resid(model)
error

## Normality Test
lillie.test(error)

# H0: error berdistribusi normal
# H1: error tidak berdistribusi normal
## P-Value > 0.05, maka gagal tolak H0 dan data berdistribusi Normal
# Kita mau dia normal


## Homoscedasticity
bptest(model)

# H0: Error mempunyai variance konstan (homoscedastic)
# H1: error tidak mempunyai variance konstan (heteroskedastis)
## P-value > 0.05, gagal tolak maka mempunyai variance yang constant
# yang kita mau itu dia homoscedastic


## Auto Correlation
dwtest(model) # Problem ada di sini

# H0: Error Tidak berautokorelasi
# H1: Error berautokorelasi
## P-Value > 0.05, kita gagal tolak dan error tidak berautokorelasi
# Yang kita mau dia itu tidak berautokorelasi


## Multicollinearity
car::vif(model)
# H0: Tidak terjadi multikolinearitas
# H1: terjadi multikolinearitas
# Angka lebih besar dari 10 maka dia mempunyai multicollinearity (menolak H0)


## Model Pembetul
# Autocorrelation
model_orcutt <- cochrane.orcutt(model, convergence = 8, max.iter = 100)
summary(model_orcutt)

residuals_orc <- residuals(model_orcutt)
fitted_values <- fitted(model_orcutt)

# Calculate the residual sum of squares (RSS)
rss <- sum(residuals_orc^2)

# Calculate the number of parameters in the model
num_parameters <- length(coef(model_orcutt))

# Calculate the AIC
n <- length(data$human_development_index)
aic <- n * log(rss/n) + 2 * num_parameters

# Print the AIC score
cat("AIC Score:", aic, "\n")

error_variance <- var(residuals_orc)
print(error_variance)

log_likelihood <- sum(log(dnorm(residuals_orc)))

bic <- -2 * log_likelihood + num_parameters * log(n)

print(bic)

# Multicollinearity
model_ridge <- lmridge(data$human_development_index ~ data$avg_length_school + data$life_expectancy + data$poverty_severity_index + data$protests + 
              data$small_health_center + data$expected_year_school + data$construction_cost_index + 
              data$health_index+ data$general_doctors_count, 
              data=as.data.frame(data, scaling='sc', K=seq(0, 1, 0.001)))
summary(model_ridge)


