# Clear Env, set Working Dir.
rm(list=ls())
setwd("~/Documents/University/Geography Degree/02 Year 2/DDC/R/Practical-Week7")

# Load req. libraries
library(tidyverse)
library(plotly)
library(ggpubr)
library(ggpmisc)
library(cowplot)
library(lubridate)
library(colourpicker)
library(gganimate)
library(thermocouple)
library(Metrics)

# Load data from local CSVs
df_cal <- read_csv("NTC_cal_example.csv")
df_adc_water <- read_csv("ADC_water_bath_example.csv")
df_adc_air <- read_csv("ADC_air_logging_example.csv")
df_adc_refT <- read_csv("ADC_ref_temp.csv")

# Correct time formatting error in df_adc_refT data
df_adc_refT$Time <- parse_time(df_adc_refT$Time, "%H.%M.%S")

# Restructure the data
## ADC water bath
df_adc_water_L <- df_adc_water |>
  pivot_longer(!c(Time),
               names_to = "ADC_Bits", values_to = "Resistance")

df_adc_water_L <- df_adc_water_L |>
  mutate(min=minute(Time))

## ADC air
df_adc_air_L <- df_adc_water |>
  pivot_longer(!c(Time),
               names_to = "ADC_Bits", values_to = "Resistance")

df_adc_air_L <- df_adc_air_L |>
  mutate(min=minute(Time))

# Plot Data
p1 <- ggplot(data = df_cal, mapping = aes(x = Resistance, y = Ref_temp))
p1

p2 <- p1 + geom_point(size = 3, shape = "circle")
p2

p3 <- p1 + geom_line()
p3

p4 <- p2 + ylab("Water temperature (\u00b0c)") + xlab("Resistance (\u2126)")
p4

p5 <- p1 + geom_point(size = 3, shape = "circle", colour = "darkgoldenrod4") + ylab("Water temperature (\u00b0c)") + xlab("Resistance (\u2126)")
p5

p6 <- p1 + geom_point(size=3.5, shape=1,colour="black") + ylab("Water temperature (\u00b0C)") + xlab("Resistance (\u2126)") + theme_cowplot()
p6

# Fitting a curve to the plot (Steinhart equation)
df_cal$Temp_beta <- ThermistorTemperature(R0 = 10000, T0 = 25, R = df_cal$Resistance, betaTH = 3950)

# Plot ref temp against beta coefficient
p7 <- ggplot(data = df_cal, mapping = aes(x = Temp_beta, y = Ref_temp)) +
  geom_point(size = 3, shape = 1, colour="black") + theme_pubr()
p7

# Add variable log_res (log of Resistance)
df_cal$log_res <- log(df_cal$Resistance)

# Fit a simple linear regression model
p8 <- p4 + geom_smooth(method = lm)
p8 + stat_regline_equation(label.x=20000, label.y=40)

# Fit a polynomial
p9 <- p4 + stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), use_label("eq"), label.x = 20000, label.y = 40)
p9

# Fit a linear model to log-transformed resistance data
p10 <- ggplot(df_cal, mapping = aes(x = log_res, y = Ref_temp)) +
  geom_point(size = 3) + 
  ylab("Water temperature (\u00b0C)") + xlab("Resistance (\u2126)") +
  geom_smooth(method=lm)
p10

# Fit the calibration model
linear_model <- lm(data = df_cal, formula = Ref_temp ~ Resistance)
poly_model <- lm(data = df_cal, formula = Ref_temp ~ poly(x = Resistance, degree = 2, raw = T))

# Questions
## Can you repeat the model fitting for the log-transformed data
lin_mod_log <- lm(data = df_cal, formula = Ref_temp ~ log_res)
poly_mod_log <- lm(data = df_cal, formula = Ref_temp ~ poly(x = log_res, degree = 2, raw = T))

## Which model has the best fit to the data?
### Polynomial model

## How many coefficients do the model have and are these all significant?
### Simple linear has one, polynomial two. All significant.

# Extract the modelled temperatures for both model, add to df_cal
df_cal$Temp_linear <- linear_model$fitted.values
df_cal$Temp_poly <- poly_model$fitted.values

df_cal %>% 
  summarise(
    Mean_temp_lin = mean(df_cal$Temp_linear),
    Mean_temp_poly = mean(df_cal$Temp_poly),
    SD_temp_lin = sd(df_cal$Temp_linear),
    SD_temp_poly = sd(df_cal$Temp_poly)
  )

# Q2-4 of this section do later when writing up

# Transform df_cal into long_format to enable box plot graphing
df_cal_long <- df_cal |>
  pivot_longer(!c(Cal_medium, Resistance, Voltage, Ref_temp, log_res), names_to = "cal_model", values_to = "cal_temp")

ggplot(data = df_cal_long, mapping = aes(x = cal_model, y = cal_temp)) + 
  geom_boxplot()

ggplot(data = df_cal_long, mapping = aes(x = cal_model, y = cal_temp, colour = Cal_medium)) + 
  geom_boxplot()

ggplot(data = df_cal_long, mapping = aes(x = cal_model, y = cal_temp, colour = Cal_medium)) + 
  geom_boxplot() + 
  facet_wrap(~Cal_medium)

ggplot(data = df_cal_long, mapping = aes(x = cal_model, y = cal_temp, colour = Cal_medium)) + 
  geom_boxplot() + 
  facet_wrap(~Cal_medium, scales = "free_y")

ggplot(data = df_cal_long, mapping = aes(x = cal_model, y = cal_temp, colour = Cal_medium)) + 
  geom_jitter() + 
  facet_wrap(~Cal_medium, scales = "free_y") + 
  theme_pubclean()

# Plot calibrated temp against reference temp
ggplot(data = df_cal_long,
       mapping = aes(x=Ref_temp, y = cal_temp,
                     colour = cal_model))+geom_point(size=2)+
  facet_wrap(~cal_model,scales = "fixed")+
  theme_pubclean()+
  geom_abline(slope = 1)+
  geom_smooth(se = F,method = "lm",linetype = "dashed")+  
  stat_regline_equation(label.x=5, label.y=40)

# RMSE calc + Question for later

# ADC Data
## Calc temp from resistances calculated, using poly_model
df_adc_water_L$Temp <- predict(object = poly_model, df_adc_water_L)
df_adc_water_L$Temp_beta <- ThermistorTemperature(R0 = 10000, T0 = 25, R = df_adc_water_L$Resistance, betaTH = 3950)
df_adc_air_L$Temp <- predict(object = poly_model, df_adc_air_L)
df_adc_air_L$Temp_beta <- ThermistorTemperature(R0 = 10000, T0 = 25, R = df_adc_air_L$Resistance, betaTH = 3950)

df_adc_water_L |>
  ggplot(aes(x = Time, y = Temp, colour = ADC_Bits)) + 
  geom_point(size = 0.7)

df_adc_water_L |>
  ggplot(aes(x = Time, y = Temp, colour = ADC_Bits)) + 
  geom_point(size = 0.7) + 
  facet_wrap(~ADC_Bits)

p11 <- df_adc_water_L |>
  ggplot(aes(x = Time, y = Temp, colour = ADC_Bits)) + 
  geom_line() + 
  facet_wrap(~ADC_Bits)

## Q1-4 answer later

# Interactive plotting
ggplotly(p11, width = 600, height = 400)

## Q1-3 answer later

# Air temp logging
df_adc_ref_water <- df_adc_refT |> filter(Medium == "Water") |> left_join(df_adc_water_L, by = "Time")

df_adc_ref_air <- df_adc_refT |> filter(Medium == "Air") |> left_join(df_adc_air_L, by = "Time")

df_adc_ref_air |> 
  ggplot(aes(x = ADC_Bits, y = Temp_beta)) + 
  geom_boxplot() + 
  geom_point()
