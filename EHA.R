### EHA with coxph ###


install.packages("survival")
library("survival")
install.packages("ggplot2")
library(ggplot2)  # For data visualization
install.packages("car")
install.packages("carData")
library(carData)
library(car)
install.packages("rms")
library(rms)

# Read the data into a data frame
data <- file.choose()

attach(data)

### Learning model 
#descriptive stats
install.packages("vtable")
library(kableExtra)
library(vtable)

st(data)

#Kaplan-Meier non parametric analysis
kmsurvival <- survfit(Surv(data$survtime, data$pol_adopt) ~ 1)
summary(kmsurvival)


plot(kmsurvival, xlab = "Time", ylab = "Survival Probability")

# Compute the Nelson-Aalen estimator
na_survival <- survfit(Surv(data$survtime, data$pol_adopt) ~ 1, type = "fh")

plot(na_survival, xlab = "Time", ylab = "Survival Probability")


# Print the summary of the Nelson-Aalen estimator
summary(na_survival)


### All variables together

# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(survtime1) ~ preci + drought + pop_growth 
               + farm_val + gov_ideo + Sierra +reg_diff
               + ideo_diff + water_disp  + pop_dens, method = "breslow")
summary(coxph)

# Compute the Type 3 ANOVA with a Wald test
anova_result <- Anova(coxph, type = "III")

# Print the results
print(anova_result)


cox.zph(coxph) 
aicC <- AIC(coxph)
aicC
bicC <- BIC(coxph)
bicC

coxpht <- coxph(Surv(survtime1) ~ preci + drought + pop_growth
                + tt(farm_val) + gov_ideo + Sierra +reg_diff
                + ideo_diff + water_disp  + pop_dens, method = "breslow")
summary(coxpht)

# Compute the Type 3 ANOVA with a Wald test
anova_result <- Anova(coxpht, type = "III")

# Print the results
print(anova_result)


aicCt <- AIC(coxpht)
aicCt
bicCt <- BIC(coxpht)
bicCt

### step-wise regression
stepwise_model <- step(coxpht, direction = "both", trace = FALSE)
summary(stepwise_model)

aicCtsw <- AIC(stepwise_model)
aicCtsw
bicCtsw <- BIC(stepwise_model)
bicCtsw


# Compute the survival probabilities over time
surv_prob <- survfit(coxph)

# Create a data frame with time and fraction of adopters
df <- data.frame(time = surv_prob$time, fraction = 1 - surv_prob$surv)

# Plot the fraction of adopters over time
ggplot(df, aes(x = time, y = fraction)) +
  geom_step() +
  xlab("Time") +
  ylab("Fraction of Adopters")

# Plot the fraction of adopters over time with a smoothed curve
ggplot(df, aes(x = time, y = fraction)) +
  geom_step() +
  geom_smooth(method = "glm", se = FALSE, color = "blue") +
  xlab("Time") +
  ylab("Fraction of Adopters")


# Learning model 
coxphL <- coxph(Surv(data$survtime,data$pol_adopt) ~ preci + drought + pop_growth 
                + tt(farm_val) + gov_ideo + Sierra
                + reg_diff + pop_dens, method = "breslow")
summary(coxphL)



# Compute the Type 3 ANOVA with a Wald test
anova_result <- Anova(coxphL, type = "III")

# Print the results
print(anova_result)

aicL <- AIC(coxphL)
aicL
bicL <- BIC(coxphL)
bicL

### Emulation model 
# Cox proportional hazard model - coefficients and hazard rates
coxphE <- coxph(Surv(data$survtime,data$pol_adopt) ~ preci + drought + pop_growth 
                + tt(farm_val) + gov_ideo + Sierra
                + ideo_diff + pop_dens, method = "breslow")
summary(coxphE)


# Compute the Type 3 ANOVA with a Wald test
anova_result <- Anova(coxphE, type = "III")

# Print the results
print(anova_result)

aicE <- AIC(coxphE)
aicE
bicE <- BIC(coxphE)
bicE

### Competition model
# Cox proportional hazard model - coefficients and hazard rates
coxphC <- coxph(Surv(data$survtime,data$pol_adopt) ~ preci + drought + pop_growth 
                + farm_val + gov_ideo + Sierra
                + water_disp + pop_dens, method = "breslow")
summary(coxphC)

# Compute the Type 3 ANOVA with a Wald test
anova_result <- Anova(coxphC, type = "III")

# Print the results
print(anova_result)

aicC <- AIC(coxphC)
aicC
bicC <- BIC(coxphC)
bicC


################################################################################

WeibullM <- survreg(Surv(survtime1, pol_adopt) ~ preci + drought + pop_growth 
                    + farm_val + gov_ideo + Sierra +reg_diff
                    + ideo_diff + water_disp  + pop_dens, dist = "weibull")
summary(WeibullM)
aicW <- AIC(WeibullM)
aicW
bicW <- BIC(WeibullM)
bicW
### step-wise regression
stepwise_model <- step(WeibullM, direction = "both", trace = FALSE)
summary(stepwise_model)
AIC(stepwise_model)
BIC(stepwise_model)

# Compute Martingale residuals
# Get the estimated coefficients and shape parameter
coef_est <- coef(WeibullM)
shape_est <- WeibullM$scale

# Compute the linear predictor
linear_pred <- as.vector(model.matrix(WeibullM) %*% coef_est)

# Compute the expected hazard function
expected_hazard <- shape_est * exp(linear_pred)^(shape_est - 1)

# Compute the Martingale residuals
martingale_res <- (pol_adopt - expected_hazard) / sqrt(expected_hazard)

# Print the Martingale residuals
print(martingale_res)

# Compute Deviance residuals
deviance_res <- residuals(WeibullM, type = "deviance")
deviance_res

# Compute Schoenfeld residuals
schoenfeld_res <- residuals(WeibullM, type = "matrix")
schoenfeld_res


# Fit log-normal regression model
lognormal_model <- survreg(Surv(survtime1) ~ preci + drought + pop_growth 
                           + farm_val + gov_ideo + Sierra +reg_diff
                           + ideo_diff + water_disp  + pop_dens, data = data, dist = "lognormal")
# View model summary
summary(lognormal_model)
aicL <- AIC(lognormal_model)
aicL
bicL <- BIC(lognormal_model)
bicL

# Fit log-logistic regression model
loglogistic_model <- survreg(Surv(survtime1) ~ preci + drought + pop_growth 
                             + farm_val + gov_ideo + Sierra +reg_diff
                             + ideo_diff + water_disp  + pop_dens, data = data, dist = "loglogistic")
# View model summary
summary(loglogistic_model)
aicLL <- AIC(loglogistic_model)
aicLL
bicLL <- BIC(loglogistic_model)
bicLL




####################################### Extra notes ####################################################
# Compute the Type 3 ANOVA with a Wald test
anova_result <- Anova(coxph, type = "III")

# Print the results
print(anova_result)



# Perform Grambsch-Therneau tests
gt_tests <- cox.zph(coxph)

# Plot Schoenfeld residuals for each variable
for (variable_name in colnames(data)) {
  if (variable_name != "survtime" && variable_name != "pol_adopt") {
    plot(gt_tests[4,variable_name])
  }
}

