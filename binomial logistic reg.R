### Logistic Regression Binomial ###
detach(data)
# Read the data into a data frame
data <- file.choose()

## Correlation plot

install.packages("ggplot2")
library(ggplot2)  # For data visualization
install.packages("reshape2")
library(reshape2)

attach(data)

# check for correlation between the independent variables
subset_data <- data[c("preci", "drought", "pop_growth", 
                       "farm_val", "gov_ideo", "Sierra", "pop_dens")]
summary(subset_data)

cor_matrix <- cor(subset_data)

# turn the cor_matrix to a table
# Convert the correlation matrix into a table format
cor_table <- as.table(round(cor_matrix, digits = 2))

# Print the correlation table
print(cor_table)

## Creating a heat map of the correlation
cor_data <- reshape2::melt(cor_matrix)


cor_plot <- ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limits = c(-1,1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()


print(cor_plot)

# Fit univariate logistic regressions
model1 <- glm(pol_adopt ~ preci, data = data, family = binomial())
model2 <- glm(pol_adopt ~ drought, data = data, family = binomial())
model3 <- glm(pol_adopt ~ pop_growth, data = data, family = binomial())
model4 <- glm(pol_adopt ~ farm_val, data = data, family = binomial())
model5 <- glm(pol_adopt ~ gov_ideo, data = data, family = binomial())
model6 <- glm(pol_adopt ~ Sierra, data = data, family = binomial())
model7 <- glm(pol_adopt ~ pop_dens, data = data, family = binomial())

# Print the model summaries
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)

## Initial model including all variables created
model <- glm(pol_adopt ~ preci + drought + pop_growth + farm_val
             + gov_ideo + Sierra + pop_dens, 
             data = data, family = "binomial")
summary(model)


# calculate the Variance Inflation Factor (VIF) for each independent variable in your glm()
install.packages("car")
library(car)
vif_values <- vif(model)
print(vif_values, digits = 2)

bic <- BIC(model)
bic
aic <- AIC(model)
aic

## stepwise regression analysis
library(MASS)
step_model <- stepAIC(model, direction = "both")
summary(step_model)

bic <- BIC(step_model)
bic
aic <- AIC(step_model)
aic

install.packages("vtable")
library(vtable)
st(data)

library(ggplot2)


#### Robustness check with EHA 
### We include ALL the external driving factors in the LR
modelR <- glm(pol_adopt ~ preci + drought + pop_growth + farm_val
             + gov_ideo + Sierra + pop_dens + reg_diff + ideo_diff + water_disp,
             data = data, family = "binomial")
summary(modelR)
bic <- BIC(modelR)
bic
aic <- AIC(modelR)
aic

step_modelR <- stepAIC(modelR, direction = "both")
summary(step_modelR)


## we do Learning model
modelL <- glm(pol_adopt ~ preci + drought + pop_growth + farm_val
              + gov_ideo + Sierra + pop_dens + reg_diff,
              data = data, family = "binomial")
summary(modelL)
bic <- BIC(modelL)
bic
aic <- AIC(modelL)
aic


##we do the emulation model
modelE <- glm(pol_adopt ~ preci + drought + pop_growth + farm_val
              + gov_ideo + Sierra + pop_dens + ideo_diff,
              data = data, family = "binomial")
summary(modelE)
bic <- BIC(modelE)
bic
aic <- AIC(modelE)
aic

## we do the competition model
modelC <- glm(pol_adopt ~ preci + drought + pop_growth + farm_val
              + gov_ideo + Sierra + pop_dens + reg_diff,
              data = data, family = "binomial")
summary(modelC)
bic <- BIC(modelC)
bic
aic <- AIC(modelC)
aic

