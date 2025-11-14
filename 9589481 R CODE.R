
# Loading packages
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)

# Importing the Excel file
g_warming_data <- read_excel("Global Warming Analysis to Temperature and CO 2 Emission combined data.xlsx")

# Viewing the data
head(g_warming_data)

# Summary of the data
summary(g_warming_data)


# Fitting linear regression model
model <- lm(`Temp Change` ~ `Total Emissions`, data = g_warming_data)

# View model summary
summary(model)

# Scatter plot with trend line
ggplot(g_warming_data, aes(x = `Total Emissions`, y = `Temp Change`)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship Between Total CO2 Emissions and Global Temperature",
       x = "CO2 Emissions", y = "Temperature Change (°C)") +
  theme_minimal()


# Extracting residuals and fitted values
residuals <- resid(model)
fitted <- fitted(model)


# Residuals vs. Fitted Values
plot(fitted, residuals,
     main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lty = 2)

# Normal Q-Q Plot
qqnorm(residuals, main = "Normal Q-Q Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)

#statistical inferences on the model
anova(model)

confint(model, level = 0.95)


