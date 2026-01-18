## R Script for Data Cleaning, Exploratory Analysis, and Statistical Modelling of NO2 Air Pollution Data in Sheffield##

##Data Gathering and Cleaning##

#required package download
#install.packages(c(
#  "tidyverse", "janitor", "naniar", "broom", "zoo", "writexl",
#  "correlation", "corrplot", "FSA", "car", "lmtest", "sandwich",
#  "relaimpo", "openair", "forecast", "ranger", "pdp"))

#loading required packages
library(tidyverse)      # Includes ggplot2, dplyr, readr, tidyr, etc.
library(janitor)        # For clean_names()
library(naniar)         # For gg_miss_var()
library(broom)          # For converting models to dataframes (tidy())

# Imputation and Export of files
library(zoo)            # For na.approx()
library(writexl)        # For writing Excel files

# Statistical Analysis
library(correlation)    # For correlation() tables
library(corrplot)       # For visual correlation heatmaps
library(FSA)            # For Dunn's Test
library(car)            # For vif()
library(lmtest)         # For coeftest()
library(sandwich)       # For NeweyWest() standard errors
library(relaimpo)       # For dominance analysis (calc.relimp)

# Time Series and Pollution Specific
library(openair)        # For polarPlot, calendarPlot, smoothTrend
library(forecast)       # For ggAcf

# Machine Learning
library(ranger)         # For Random Forest model
library(pdp)            # For Partial Dependence Plots

#Set working directionary - for example
#setwd("c:\\Users\\Andrzej user\\Documents\\Data Science\\Introduction to Data Science\\Coursework\\Working Directory")

#Import air pollution and weather data csv files
#Remember to have the downloaded csv files in the correct working directory
air_pollution_raw <- read_csv("air pollution complete.csv", na = c("NA", "No data"))
weather_raw <- read_csv("weather data.csv")

#View inputted files
#View(weather_raw)
#View(air_pollution_raw)

#Clean weather data
weather_clean <- weather_raw |> janitor::clean_names()
#Change date format to Posixct
weather_clean$time <- as.Date(weather_clean$time, format = "%d/%m/%Y")
#Rename time column to date to be consistent with air pollution data
weather_clean <- weather_clean %>%
  rename(date = time)
#Create wind direction categorical variable based on wind direction degrees - easier for analysis
weather_clean <- weather_clean %>%
  mutate(
    wind_direction = case_when(
      wind_direction_10m_dominant >= 337.5 | wind_direction_10m_dominant < 22.5 ~ "N",
      wind_direction_10m_dominant >= 22.5  & wind_direction_10m_dominant < 67.5 ~ "NE",
      wind_direction_10m_dominant >= 67.5  & wind_direction_10m_dominant < 112.5 ~ "E",
      wind_direction_10m_dominant >= 112.5 & wind_direction_10m_dominant < 157.5 ~ "SE",
      wind_direction_10m_dominant >= 157.5 & wind_direction_10m_dominant < 202.5 ~ "S",
      wind_direction_10m_dominant >= 202.5 & wind_direction_10m_dominant < 247.5 ~ "SW",
      wind_direction_10m_dominant >= 247.5 & wind_direction_10m_dominant < 292.5 ~ "W",
      wind_direction_10m_dominant >= 292.5 & wind_direction_10m_dominant < 337.5 ~ "NW",
      TRUE ~ NA_character_
    )
  )
weather_clean$wind_direction <- as.factor(weather_clean$wind_direction)
#Rename variables for easier reference
weather_clean <- weather_clean %>%
  rename(
    wmo_weather_code = weather_code_wmo_code,
    wind_gusts_max = wind_gusts_10m_max_km_h,
    wind_gusts_mean = wind_gusts_10m_mean_km_h,
    wind_speed_max = wind_speed_10m_max_km_h,
    wind_speed_mean = wind_speed_10m_mean_km_h,
    shortwave_radiation = shortwave_radiation_sum_mj_m2,
    pressure = pressure_msl_mean_h_pa,
    cape = cape_mean_j_kg,
    humidity = relative_humidity_2m_mean_percent,
    temperature = temperature_2m_mean_c,
    precipitation = precipitation_sum_mm,
    sunshine_duration = sunshine_duration_s,
    daylight_duration = daylight_duration_s,
    wind_direction_n = wind_direction_10m_dominant
  )

#Check cleaned weather data
#View(weather_clean)
#View(summary(weather_clean))

#Clean air pollution data
#Remove unnecessary rows which either contain empty or irrelevant information
air_pollution_clean <- air_pollution_raw %>%
  slice(-(1:9))
air_pollution_clean <- air_pollution_clean %>%
  slice(-(2:7))
air_pollution_clean <- air_pollution_clean %>%
  slice(-1)
air_pollution_clean <- air_pollution_clean %>%
  slice(1:(n()-4))
#Clean up and standardise column names
air_pollution_clean <- air_pollution_clean %>%
  row_to_names(row_number = 1, remove_row = TRUE, remove_rows_above = TRUE) %>%
  clean_names()
#Remove unnecessary Status columns
air_pollution_clean <- air_pollution_clean %>%
  dplyr::select(-c(3, 5, 7, 9, 11, 13, 15, 17))
names(air_pollution_clean)
air_pollution_clean <- air_pollution_clean |> janitor::clean_names()
#Change date format to Posixct
air_pollution_clean$date <- as.Date(air_pollution_clean$date, format = "%d/%m/%Y")
#View(air_pollution_clean)

#Rename gas name columns for easier reference
air_pollution_clean <- air_pollution_clean %>%
  rename(
    pm2_5 = pm2_5_particulate_matter_hourly_measured,
    pm2_5_1 = pm2_5_particulate_matter_hourly_measured_2,
    pm2_5_2 = pm2_5_particulate_matter_hourly_measured_3,
    NO2 = nitrogen_dioxide,
    NO2_1 = nitrogen_dioxide_2,
    NO2_2 = nitrogen_dioxide_3,
    ozone_1 = ozone
  )

#Code the numeric columns as numeric variables in R
air_pollution_clean <- air_pollution_clean %>%
  mutate(
    NO2 = as.numeric(NO2),
    pm2_5 = as.numeric(pm2_5),
    ozone_1 = as.numeric(ozone_1),
    NO2_1 = as.numeric(NO2_1),
    pm2_5_1 = as.numeric(pm2_5_1),
    ozone_2 = as.numeric(ozone_2),
    NO2_2 = as.numeric(NO2_2),
    pm2_5_2 = as.numeric(pm2_5_2)
  )

#Check for anomolous outliers
#View(summary(air_pollution_clean))

#Remove anomolous outliers for PM2.5 variables
table(air_pollution_clean$pm2_5 > 20)
air_pollution_clean <- air_pollution_clean %>%
  mutate(
    pm2_5 = ifelse(pm2_5 > 36, 36, pm2_5),
    pm2_5_1 = ifelse(pm2_5_1 > 36, 36, pm2_5_1),
    pm2_5_2 = ifelse(pm2_5_2 > 36, 36, pm2_5_2)
  )

#Create Column averages for each air pollution gas and inserting them into a daily average column
#1, Code the NO2 average
air_pollution_clean <- air_pollution_clean |>
  mutate(
    across(
      starts_with("NO2"),
      ~ as.numeric(as.character(.x))
    )
  ) |>
  rowwise() |>
  mutate(
    NO2_average = mean(
      c_across(starts_with("NO2")),
      na.rm = TRUE
    )
  ) |>
  ungroup()

#2, Code the PM2.5 average
air_pollution_clean <- air_pollution_clean |>
  mutate(
    across(
      starts_with("pm2_5"),
      ~ as.numeric(as.character(.x))
    )
  ) |>
  rowwise() |>
  mutate(
    pm2_5_average = mean(
      c_across(starts_with("pm2_5")),
      na.rm = TRUE
    )
  ) |>
  ungroup()

#Code the ozone average
air_pollution_clean <- air_pollution_clean |>
  mutate(
    across(
      starts_with("ozone"),
      ~ as.numeric(as.character(.x))
    )
  ) |>
  rowwise() |>
  mutate(
    ozone_average = mean(
      c_across(starts_with("ozone")),
      na.rm = TRUE
    )
  ) |>
  ungroup()

#Test for missing variables in the air pollution data frame
gg_miss_var(air_pollution_clean, show_pct = TRUE) + 
  theme_minimal() +                          
  labs(
    title = "Missing Data by Variable - air_pollution_clean",
    subtitle = "Percentage of missing entries in the air_pollution_clean dataset by variable",
    y = "Percent Missing (%)"
  ) +
  theme(
    axis.text.y = element_text(size = 17, face = "bold"),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 18)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))

#Create a new data frame with only date and average air pollution variables for further analysis. This helps makes the joint
#air pollution and weather data frame cleaner
air_pollution_clean_averages <- air_pollution_clean %>%
  dplyr::select(date, NO2_average, pm2_5_average, ozone_average)
#View(air_pollution_clean_averages)

#Test for missing variables in the air pollution averages data frame
gg_miss_var(air_pollution_clean_averages)

#Code in missing values for the ozone variable using the zoo package
air_pollution_clean_averages$ozone_average <- na.approx(air_pollution_clean_averages$ozone_average,maxgap = 2)
#Air pollution averages variable now contains no missing data

#Merge air pollution averages and weather data
sheffield_data <- inner_join(air_pollution_clean_averages, weather_clean, by = "date")

#Check for missing values for combined dataset
gg_miss_var(sheffield_data, show_pct = TRUE) + 
  theme_minimal() +                          
  labs(
    title = "Missing Data by Variable - sheffield_data",
    subtitle = "Percentage of missing entries in the sheffield_data dataset by variable",
    y = "Percent Missing (%)"
  ) +
  theme(
    axis.text.y = element_text(size = 17, face = "bold"),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.32),
    plot.subtitle = element_text(size = 18, hjust = 0.20, color = "grey40"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22, hjust = 0.35),
    axis.text.x = element_text(size = 18)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))
#View(sheffield_data)
#Create train and test data frames
sheffield_data_train <- sheffield_data %>%
  filter(date >= "2022-01-01" & date <= "2024-12-31")
#View(sheffield_data_train)
sheffield_data_test <- sheffield_data %>%
  filter(date > "2024-12-31")
#View(sheffield_data_test)

### Exploratory analysis ###

#Check for missing values
gg_miss_var(sheffield_data_train, show_pct = TRUE) + 
  theme_minimal() +                          
  labs(
    title = "Missing Data by Variable - sheffield_data_train",
    subtitle = "Percentage of missing entries in the air_pollution_clean dataset by variable",
    y = "Percent Missing (%)"
  ) +
  theme(
    axis.text.y = element_text(size = 17, face = "bold"),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 18)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))

gg_miss_var(sheffield_data_train)

#Visualise average air quality variables
#NO2
ggplot(sheffield_data_train, aes(x = NO2_average)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Daily NO2 Levels", x = "NO2 (µg/m³)")
#PM2.5
ggplot(sheffield_data_train, aes(x = pm2_5_average)) +
  geom_histogram(binwidth = 2, fill = "darkgreen", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Daily PM2.5 Levels", x = "PM2.5 (µg/m³)")
#Ozone
ggplot(sheffield_data_train, aes(x = ozone_average)) +
  geom_histogram(binwidth = 2, fill = "orange", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Daily Ozone Levels", x = "Ozone (µg/m³)")

#Test for normality of air pollution variables using Shapiro-Wilk test
shapiro_no2 <- shapiro.test(sheffield_data_train$NO2_average)
shapiro_pm25 <- shapiro.test(sheffield_data_train$pm2_5_average)
shapiro_ozone <- shapiro.test(sheffield_data_train$ozone_average)
shapiro_no2
shapiro_pm25
shapiro_ozone

#Test for Autocorrelation - important for future linear regression analysis
#Test for Autocorrelation of NO2 average variable 
#ACF plot
ggAcf(sheffield_data_train$NO2_average,
    na.action = na.omit,
    lag.max = 30) + theme_minimal() +
  labs(
    title = "Autocorrelation of NO2",
    subtitle = "Dashed lines represent 95% confidence limits",
    y = "Correlation",
    x = "Lag (Days)"
  ) + 
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5), 
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    plot.subtitle = element_text(size = 18)
  )

#Obtain p-value
Box.test(sheffield_data_train$NO2_average, type = "Ljung-Box")
#P-value < 0.05 so there is significant autocorrelation present in NO2 average variable

#Test for autocorrelation of PM2.5 average variable
#ACF plot
ggAcf(sheffield_data_train$pm2_5_average,
    na.action = na.omit,
    lag.max = 30) + theme_minimal() +
  labs(
    title = "Autocorrelation of pm2.5",
    subtitle = "Dashed lines represent 95% confidence limits",
    y = "Correlation",
    x = "Lag (Days)"
  ) + 
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5), 
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    plot.subtitle = element_text(size = 18)
  )

#Obtain p-value
Box.test(sheffield_data_train$pm2_5_average, type = "Ljung-Box")
#P-value < 0.05 so there is significant autocorrelation present in pm2.5 average var


#Test for autocorrelation of ozone average variable
#ACF plot
ggAcf(sheffield_data_train$ozone_average,
    na.action = na.omit,
    lag.max = 30) + theme_minimal() +
  labs(
    title = "Autocorrelation of O3",
    subtitle = "Dashed lines represent 95% confidence limits",
    y = "Correlation",
    x = "Lag (Days)"
  ) + 
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5), 
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    plot.subtitle = element_text(size = 18)
  )

#Obtain p-value
Box.test(sheffield_data_train$ozone_average, type = "Ljung-Box")
#P-value < 0.05 so there is significant autocorrelation present in the ozone average variable


#All Shapiro-Wilk tests return p-values < 0.05, indicating non-normal distribution of air pollution variables
#Therefore, non-parametric correlation tests will be used for further analysis

###Data Analysis - RQ1###
##NO2 Analysis Only##

#Remove non-NO2 air pollution variables from sheffield data train
sheffield_data_train <- sheffield_data_train %>%
  dplyr::select(-c(
    pm2_5_average,
    ozone_average,
  ))

#Create correlation heatmap
numeric_data <- sheffield_data_train %>%
  dplyr::select(where(is.numeric)) %>%
  na.omit()
firstcorrelation <- cor(numeric_data)
firstcorrelationplot <- corrplot(
    firstcorrelation, method = "color",
    type = "upper", tl.col = "black",
    tl.cex = 1.1, tl.srt = 45, diag=FALSE,
    cl.cex = 1.2
)

#Create correlation values table
Correlation_table <- correlation(numeric_data, method = "spearman", p_adjust = "holm")
#View(Correlation_table)

#Create correlation value table for NO2 - insignificant correlations (adjusted p (Holm) > 0.05) are filtered out
NO2correlationTable <- correlation(
    numeric_data$NO2_average, 
    numeric_data, 
    method = "spearman",
    p_adjust = "holm" 
  ) %>%
  mutate(Parameter1 = "NO2_average") %>%
  filter(p < 0.05) %>%
  arrange(desc(abs(rho)))
#View(NO2correlationTable)
#Save table to excel
#write_xlsx(NO2correlationTable, "NO2correlationTable.xlsx")

#Undertake Kruskal-Wallis analysis of NO2 levels by wind direction
NO2_wind_dir_kw <- kruskal.test(
  NO2_average ~ wind_direction,
  data = sheffield_data_train
)
NO2_wind_dir_kw
#p < 0.05 so there is a significant difference in NO2 levels in atleast one wind direction
#Post-hoc Dunn's test to identify which wind directions have significant differences in NO2 levels
NO2_wind_dir_dunn <- dunnTest(
  NO2_average ~ wind_direction,
  data = sheffield_data_train,
  method = "holm"
)
NO2_wind_dir_dunn

#Visualise NO2 levels by wind direction using boxplot
NO2_wind_dir_boxplot <- ggplot(sheffield_data_train, aes(x = wind_direction, y = NO2_average)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "NO2 Levels by Wind Direction",
       x = "Wind Direction",
       y = "NO2 Levels (µg/m³)") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    plot.title = element_text(size = 25, hjust = 0.5)
  )
NO2_wind_dir_boxplot

#Visualise wind direction and speed together with NO2 levels using polar plot
KW_data <- sheffield_data_train %>%
  rename(wd = wind_direction_n, ws = wind_speed_mean, no2 = NO2_average)
NO2_WSandWD_plot <- polarPlot(KW_data, pollutant = "no2", 
          main = "NO2 Levels by Wind Direction & Speed") + 
  theme(
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        plot.title = element_text(size = 25, hjust = 0.5)
      )
NO2_WSandWD_plot

#Analyse NO2 autocorrelation and variance with time
#Create time analysis plots of NO2 levels using openair package
#Calendar plot for NO2 levels in 2022
NO2_calendar_2022 <- calendarPlot(sheffield_data_train, 
   pollutant = "NO2_average", 
   year = 2022)
NO2_calendar_2022
#Smooth trend analysis for NO2 levels from 2022-2024
smoothTrend(sheffield_data_train, 
  pollutant = "NO2_average", 
  xlab = "Date", 
  ylab = "NO2 Levels (µg/m³)", 
  main = "NO2 Trend Analysis 2022-2024",
  fontsize = 18
)

###Data Analysis - RQ2 and RQ3###

#Create multilinear regression (mlr) model of NO2 levels using all correlated variables
NO2_mlr_lm <- lm(NO2_average ~ wind_speed_mean + wind_direction + wind_gusts_mean +
  wind_speed_max + wind_gusts_max + uv_index_max + cape + shortwave_radiation + wind_direction_n +
  sunshine_duration + pressure +
  temperature + daylight_duration + 
  humidity, data = sheffield_data_train)
NO2_mlr_lm
vif(NO2_mlr_lm)
coeftest(NO2_mlr_lm, vcov = NeweyWest(NO2_mlr_lm))
summary(NO2_mlr_lm)

#Q-Q plot
#Creating a dataframe with residuals for Q-Q plot
sheffield_data_NO2_full_lm <- sheffield_data_train %>%
  dplyr::select(date, NO2_average)
sheffield_data_NO2_full_lm$residuals_mlv_NO2 <- residuals(NO2_full_multivariate_lm)
# Q-Q plot of residuals to check for normality
ggplot(sheffield_data_NO2_full_lm, aes(sample = residuals_mlv_NO2)) +
  stat_qq(size = 2, color = "blue", alpha = 0.5) + 
  stat_qq_line(color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(
    title = "Normal Q-Q Plot of Residuals",
    subtitle = "Points should fall along the red dashed line",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    plot.title = element_text(size = 25, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40")
  )

#Coefficients
#Visualise the coefficients of each variable in the full multilinear model using a coefficient plot

coefficient_data <- tidy(NO2_mlr_lm, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% 
  mutate(term = reorder(term, estimate),
  direction = ifelse(estimate >= 0, "Positive", "Negative")
)

ggplot(coefficient_data, aes(x = estimate, y = term, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = direction), 
                width = 0.2) +
  geom_point(aes(color = direction),size = 3) +
  geom_text(aes(label = round(estimate, 2)), 
            vjust = -0.8,
            size = 5.5) +
  scale_color_manual(values = c("Positive" = "blue", "Negative" = "red")) +
  labs(
    title = "Coefficients of variables in mlr model",
    subtitle = "Dependent Variable: Daily Mean NO2",
    x = "Coefficient value in estimating NO2",
    y = "Predictor Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 18)
) +
  theme(legend.position = "none")

#vMeasure multicollinearity of mlr model using VIF
mlr_vif <- vif(NO2_mlr_lm)
mlr_vif
#Creat vif table for analysis
mlr_vif_table <- as.data.frame(mlr_vif)
mlr_vif_table$variable <- rownames(mlr_vif_table)
mlr_vif_table <- mlr_vif_table %>%
  janitor::clean_names() %>%
  dplyr::select(variable, everything())
mlr_vif_table_sorted <- mlr_vif_table %>%
  arrange(desc(gvif_1_2_df))
#Save vif table to excel
#write_xlsx(mlr_vif_table_sorted, "mlr_vif_sorted.xlsx")

#Test how well the full multivariate model performs on the test dataset
predict(NO2_mlr_lm, newdata = sheffield_data_test, interval = "confidence")
sheffield_data_test_NO2_mlr_lm <- sheffield_data_test %>%
  dplyr::select(date, NO2_average)
sheffield_data_test_NO2_mlr_lm$predicted_NO2 <- predict(NO2_mlr_lm, newdata = sheffield_data_test)
sheffield_data_test_NO2_mlr_lm$residuals_NO2 <- sheffield_data_test_NO2_mlr_lm$NO2_average - sheffield_data_test_NO2_mlr_lm$predicted_NO2
#View(sheffield_data_test_NO2_mlr_lm)

#Graph predicted vs actual NO2 levels for test dataset
NO2_test_predicted_vs_actual_plot <- ggplot(sheffield_data_test_NO2_mlr_lm, aes(x = predicted_NO2, y = NO2_average)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Predicted vs Actual NO2 Levels (Test Dataset)",
    x = "Predicted NO2 Levels (µg/m³)",
    y = "Actual NO2 Levels (µg/m³)") +
  theme_minimal()
NO2_test_predicted_vs_actual_plot

#Calculate RMSE for test dataset predictions
NO2_test_rmse <- sqrt(mean(sheffield_data_test_NO2_mlr_lm$residuals_NO2^2, na.rm = TRUE))
NO2_test_rmse
sd(sheffield_data_test$NO2_average)
mean(sheffield_data_test$NO2_average)

##random forest##

#Create random forest (RF) model for NO2 levels
#Set seed for reproducibility - this ensures the same random sampling is used each time the model is run
set.seed(123)
NO2_rf_model <- ranger(
  formula = NO2_average ~ wind_speed_mean + wind_direction + wind_gusts_mean +
  wind_speed_max + wind_gusts_max + uv_index_max + cape + shortwave_radiation + 
  sunshine_duration + pressure + wind_direction_n +
  temperature + daylight_duration + 
  humidity, 
  data = sheffield_data_train,
  importance = 'impurity'
)
#Analyse the random forest model
print(NO2_rf_model)

#Test the predictive power of the rf model
NO2_rf_model_predict <- predict(NO2_rf_model, data = sheffield_data_test)$predictions
sheffield_data_test_NO2_rf <- sheffield_data_test %>%
  dplyr::select(date, NO2_average)
sheffield_data_test_NO2_rf$predicted_NO2_rf <- NO2_rf_model_predict
sheffield_data_test_NO2_rf$residuals_NO2_rf <- sheffield_data_test_NO2_rf$NO2_average - sheffield_data_test_NO2_rf$predicted_NO2_rf
#View(sheffield_data_test_NO2_rf)
#Calculate RMSE for the random forest model on the test dataset
NO2_rf_test_rmse <- sqrt(mean(sheffield_data_test_NO2_rf$residuals_NO2_rf^2, na.rm = TRUE))
NO2_rf_test_rmse

##Find the non-linear NO2 variable relationships##

#Perform Dominance analysis of the importance of different variables in the mlr model
str(NO2_mlr_lm)
da_results <- calc.relimp(NO2_mlr_lm, type = c("lmg", "last", "first"), rela = TRUE)
print(da_results)
plot_data <- data.frame(
  Predictor = names(da_results$lmg), 
  Importance = da_results$lmg
)
dominance_analysis_plot <- ggplot(data = plot_data, aes(x = reorder(Predictor, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
      title = "mlr model variable importance ",
      subtitle = "using dominance analysis LMG Method",
      x = "Predictor Variable",
      y = "Relative Importance") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 18),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22)
  ) 
dominance_analysis_plot

#Plot variable importance from random forest model graph
importance_data <- data.frame(
  Variable = names(importance(NO2_rf_model)),
  Importance = importance(NO2_rf_model)
)
NO2_rf_model_importance_plot <- ggplot(importance_data, aes(x = reorder(Variable, -Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "RF model variable importance",
    x = "Predictor Variable",
    y = "Importance (Node Impurity)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 18),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22)
) 
NO2_rf_model_importance_plot

#Check whether certain variables do not work well with the linear model
#Temperature
pdp_temp <- partial(NO2_rf_model, pred.var = "temperature", train = sheffield_data_train)
pdp_temp_plot <- ggplot(pdp_temp, aes(x = temperature , y = yhat)) +
  geom_line(color = "#0073C2", size = 1.2) +
  geom_rug(
    data = sheffield_data_train, aes(x = temperature), inherit.aes = FALSE, 
    sides = "b", alpha = 0.2, length = unit(0.03, "npc")) +
  geom_point(color = "#0073C2", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Partial dependence: Temperature",
    subtitle = "Relationship between Temperature and NO2",
    x = "Temperature (°C)",
    y = "Predicted NO2 Level") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 18)
  )
pdp_temp_plot
#Temperature is a L shape (elbow) 
#Find where the L shape breaks (tipping point)
temp_steepest_slope <- pdp_temp %>%
  mutate(
    change = yhat - lag(yhat),      
    slope = change / (temperature - lag(temperature)) 
  ) %>%
  arrange(slope) %>% 
  head(5)
temp_steepest_slope


#Plot humidity partial dependence plot
pdp_humidity <- partial(NO2_rf_model, pred.var = "humidity", train = sheffield_data_train)
pdp_humidity_plot <- ggplot(pdp_humidity, aes(x =humidity , y = yhat)) +
  geom_line(color = "#0073C2", size = 1.2) +
  geom_rug(
    data = sheffield_data_train, aes(x = humidity), inherit.aes = FALSE, 
    sides = "b", alpha = 0.2, length = unit(0.03, "npc")) +
  geom_point(color = "#0073C2", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Partial dependence: Humidity",
    subtitle = "Relationship between Humidity and NO2",
    x = "Relative Humidity (%)",
    y = "Predicted NO2 Level") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 18)
  )
pdp_humidity_plot

#Plot daylight duration partial dependence plot
pdp_daylight <- partial(NO2_rf_model, pred.var = "daylight_duration", train = sheffield_data_train)
pdp_daylight_plot <- ggplot(pdp_daylight, aes(x = daylight_duration , y = yhat)) +
  geom_line(color = "#0073C2", size = 1.2) +
  geom_rug(
    data = sheffield_data_train, aes(x = daylight_duration), inherit.aes = FALSE, 
    sides = "b", alpha = 0.2, length = unit(0.03, "npc")) +
  geom_point(color = "#0073C2", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Partial Dependence: Daylight Duration",
    subtitle = "Relationship between Daylight Duration and NO2",
    x = "Daylight duration (s)",
    y = "Predicted NO2 Level") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
pdp_daylight_plot

##End of Script##