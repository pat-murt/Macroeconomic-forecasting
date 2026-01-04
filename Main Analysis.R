library(readxl) # Read/write data files
library(tidyverse) # Collection of packages
library(ggplot2) # Visualisation
library(dplyr) # Manipulating data
library(caret) # Models
library(corrplot) # Correlation Plot
library(glmnet) #LASSO
library(ggrepel) # GGPLOT label lines
library(psych) # Functions including describeBy
library(gridExtra)
library(RColorBrewer) # GGPLOT Colours
library(cluster) # For cluster analysis
library(car) # Includes VIF function
library(rpart) # Decision Trees
library(rpart.plot) # Plots for Decision Trees
library(ggfortify)
library(flextable)
library(knitr)
library(writexl)
library(grid)
library(ggcorrplot)
library(tfdatasets)
library(caretEnsemble)
library(iml)

#Set working directory
setwd('E:/Dissertation/Research Report')

#Setting preferred options
options(scipen = 999)
options(digits = 15)

#Read data in
data <- read.csv("WB_Inidicators_clean_V4.csv")

#Ensure correct format
data$country <- as.factor(data$country)
data$region <- as.factor(data$region)
data$year <- as.numeric(data$year)
data$net_migration <- as.numeric(data$net_migration)


# Creating Lag times by country and year
data <- data %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    
    access_electricity_lag3 = lag(access_electricity, n=3),
    agricultural_land_lag5 = lag(agricultural_land, n = 5),
    agricultural_forestry_fishing_lag5 = lag(agricultural_forestry_fishing, n = 5),
    co2_lag2 = lag(co2, n = 2),
    deaths_lag5 = lag(deaths, n = 5),
    forest_lag5 = lag(forest, n=5),
    life_expectancy_lag2 = lag(life_expectancy, n=2),
    net_migration_lag2 = lag(net_migration, n = 2),
    population_lag2 = lag(population, n = 2),
    urban_population_lag2 = lag(urban_population, n=2)
    
  ) %>%
  ungroup() 



########################
###  Summary Stats   ###
########################

summary(data)
describe1 <- describe(data)

#Writing dataset out for descriptive statistics
#write.csv(describe1, file = "data_describe_1.csv", row.names = FALSE)

describe(data$access_electricity_lag3)

# Viewing the spread of the data before conversion
par(mfrow=c(4,3))

hist(data$access_electricity_lag3, main = 'Access to Electricity', ylab = 'Count', xlab = 'Access to Electricity (% of population)')
rug(data$access_electricity_lag3)

hist(data$agricultural_land_lag5, main = 'Agricultural Land', ylab = 'Count', xlab = 'Agricultural Land (% of land area)')
rug(data$agricultural_land_lag5)

hist(data$agricultural_forestry_fishing_lag5, main = 'Agriculture, Forestry, Fishing', ylab = 'Count', xlab = 'Agriculture, Forestry, Fishing (% of GDP)')
rug(data$agricultural_forestry_fishing_lag5)

hist(data$co2_lag2, main = 'Co2', ylab = 'Count', xlab = 'CO2 emissions (total) (Metric tons)')
rug(data$co2_lag2)

hist(data$deaths_lag5, main = 'Deaths', ylab = 'Count', xlab = 'Deaths (per 1,000 of the population)')
rug(data$deaths_lag5)

hist(data$forest_lag5, main = 'Forest', ylab = 'Count', xlab = '% of land area covered by forest')
rug(data$forest_lag5)

hist(data$life_expectancy_lag2, main = 'Life Expectancy', ylab = 'Count', xlab = 'Life expectancy at birth (years)')
rug(data$life_expectancy_lag2)

hist(data$net_migration_lag2, main = 'Net Migration', ylab = 'Count', xlab = 'Net migration (Overall Rate)')
rug(data$net_migration_lag2)

hist(data$population_lag2, main = 'Population', ylab = 'Count', xlab = 'Overall Population')
rug(data$population_lag2)

hist(data$urban_population_lag2, main = 'Urban Population', ylab = 'Count', xlab = '% of population that lives in urban areas')
rug(data$urban_population_lag2)

hist(data$gdp, main = 'GDP', ylab = 'Count', xlab = 'GDP')
rug(data$gdp)

par(mfrow=c(1,1))


### Log Modulus Transformation

log_modulus <- function(x) {
  sign(x) * log(abs(x) + 1)
}

data$log_gdp <- log_modulus(data$gdp)
data$log_co2_lag2 <- log_modulus(data$co2_lag2)
data$log_deaths_lag5 <- log_modulus(data$deaths_lag5)
data$log_net_migration_lag2 <- log_modulus(data$net_migration_lag2)
data$log_population_lag2 <- log_modulus(data$population_lag2)

# Visualising distribution of the data after transformation

par(mfrow=c(4,3))

hist(data$access_electricity_lag3, main = 'Access to Electricity', ylab = 'Count', xlab = 'Access to Electricity (% of population)')
rug(data$access_electricity_lag3)

hist(data$agricultural_land_lag5, main = 'Agricultural Land', ylab = 'Count', xlab = 'Agricultural Land (% of land area)')
rug(data$agricultural_land_lag5)

hist(data$agricultural_forestry_fishing_lag5, main = 'Agriculture, Forestry, Fishing', ylab = 'Count', xlab = 'Agriculture, Forestry, Fishing (% of GDP)')
rug(data$agricultural_forestry_fishing_lag5)

hist(data$log_co2_lag2, main = 'Co2', ylab = 'Count', xlab = 'CO2 emissions (total) (Metric tons)')
rug(data$log_co2_lag2)

hist(data$log_deaths_lag5, main = 'Deaths', ylab = 'Count', xlab = 'Deaths (per 1,000 of the population)')
rug(data$log_deaths_lag5)

hist(data$forest_lag5, main = 'Forest', ylab = 'Count', xlab = '% of land area covered by forest')
rug(data$forest_lag5)

hist(data$life_expectancy_lag2, main = 'Life Expectancy', ylab = 'Count', xlab = 'Life expectancy at birth (years)')
rug(data$life_expectancy_lag2)

hist(data$log_net_migration_lag2, main = 'Net Migration', ylab = 'Count', xlab = 'Net migration (Overall Rate)')
rug(data$log_net_migration_lag2)

hist(data$log_population_lag2, main = 'Population', ylab = 'Count', xlab = 'Overall Population')
rug(data$log_population_lag2)

hist(data$urban_population_lag2, main = 'Urban Population', ylab = 'Count', xlab = '% of population that lives in urban areas')
rug(data$urban_population_lag2)

hist(data$log_gdp, main = 'GDP', ylab = 'Count', xlab = 'GDP')
rug(data$log_gdp)

par(mfrow=c(1,1))


################################################################################
###############                                                  ###############
###############                Visualisations                    ###############
###############                                                  ###############
################################################################################

#Subsetting first and last years for visualisations
year2001 <- subset(data, data$year == 2001)
year2021 <- subset(data, data$year == 2021)


################
###    GDP   ###
################

years <- sort(unique(data$year))
start_year <- years[1]
end_year <- years[length(years)]

gdp_change <- data %>%
  filter(year %in% c(start_year, end_year)) %>%
  group_by(country, year) %>%
  summarize(gdp = first(gdp)) %>%
  pivot_wider(names_from = year, values_from = gdp) %>%
  ungroup()

gdp_change <- gdp_change %>%
  mutate(change = `end_year` - `start_year`)

#Setting the min and max year
start_year_value <- min(data$year)
end_year_value <- max(data$year)

# Calculating the change in GDP over the timeframe
gdp_change <- data %>%
  filter(year %in% c(start_year_value, end_year_value)) %>%
  group_by(country, year) %>%
  summarize(gdp = first(gdp)) %>%
  pivot_wider(names_from = year, values_from = gdp) %>%
  mutate(change = !!sym(paste0(end_year_value)) - !!sym(paste0(start_year_value))) %>%
  ungroup()

#Showing top increases
top_increase <- gdp_change %>%
  arrange(desc(change)) %>%
  dplyr::slice(1:10)

#showing top declines
top_decline <- gdp_change %>%
  arrange(change) %>%
  dplyr::slice(1:10)

# Combining top countries for increase and decline
top_countries <- c(top_increase$country, top_decline$country)

#subsetting countries with top change
trend_data <- data %>%
  filter(country %in% top_countries)

#Organising the top changing countries
top_countries <- gdp_change %>%
  arrange(desc(change)) %>% 
  dplyr::slice(1:10) %>%
  pull(country)

# Selecting top countries
filtered_trend_data <- trend_data %>%
  filter(country %in% top_countries)

#Setting end points
end_points <- filtered_trend_data %>%
  group_by(country) %>%
  filter(year == max(year))

# Plotting the change over time as a line graph
ggplot(filtered_trend_data, aes(x = year, y = gdp / 1e9, color = country)) +
  geom_line() +
  geom_text_repel(
    data = end_points,
    aes(label = country),
    nudge_x = 0,
    nudge_y = 0,
    force = 1,
    direction = "both",
    segment.color = NA,
    max.overlaps = 10,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.curvature = 0.2
  ) +
  labs(
    title = "Top 10 Countries for GDP Growth",
    subtitle = "From 2001 - 2021",
    x = "Year", y = "GDP (Billions)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
  )



#############################
### Access to Electricity ###
#############################

electricity_2001 <- ggplot(year2001, aes(x = access_electricity, y = gdp/1e9)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +
  geom_smooth(method = 'lm', col = 'red') +
  labs(
    title = "Access to Electricity vs. GDP",
    subtitle = "Year: 2001",
    x = "Access to Electricity (%)",
    y = "GDP (US$ Billions)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(hjust = 0.5)
  )

electricity_2021 <- ggplot(year2021, aes(x = access_electricity, y = gdp/1e9)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +
  geom_smooth(method = 'lm', col = 'red') +
  labs(
    title = "Access to Electricity vs. GDP",
    subtitle = "Year: 2021",
    x = "Access to Electricity (%)",
    y = "GDP (US$ Billions)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(hjust = 0.5)
  )

grid.arrange(electricity_2001, electricity_2021, nrow = 1)


#######################
### Life expectancy ###
#######################

avg_gdp <- data %>%
  group_by(life_expectancy_lag2) %>%
  summarize(mean_gdp = mean(gdp, na.rm = TRUE))

ggplot(avg_gdp, aes(x = life_expectancy_lag2, y = mean_gdp/1e9)) +
  geom_point(color = "darkblue", size = 1, alpha = 0.7) + 
  geom_line(color = "darkblue", size = 1) +
  labs(
    title = "Average GDP Across Different Life Expectancy Levels",
    x = "Life Expectancy (years)",
    y = "Average GDP (Billions US$)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  geom_smooth(method = 'lm', col = 'red')


########################
### Urban Population ###
########################

urban_2001 <- ggplot(year2001, aes(x = urban_population, y = gdp/1e9, size = population)) +
  geom_point(alpha = 0.8, color = 'seagreen') +
  geom_smooth(method = 'lm', show.legend = FALSE) +
  labs(
    title = "Impact of Urban Population on GDP (Size=Overall Population)",
    subtitle = "Year: 2001",
    x = "Urban Population (%)",
    y = "GDP (Billions US$)",
    size = "Population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

urban_2021 <- ggplot(year2021, aes(x = urban_population, y = gdp/1e9, size = population)) +
  geom_point(alpha = 0.8, color = 'seagreen') +
  geom_smooth(method = 'lm', show.legend = FALSE) +
  labs(
    title = "Impact of Urban Population on GDP (Size=Overall Population)",
    subtitle = "Year: 2021",
    x = "Urban Population (%)",
    y = "GDP (Billions US$)",
    size = "Population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

grid.arrange(urban_2001, urban_2021, nrow = 1)


##############
### Deaths ###
##############

deaths2001 <- ggplot(year2001, aes(deaths, gdp/1e9)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', se = TRUE, color = "darkred", size = 1) +
  labs(
    title = "Relationship between Deaths and GDP",
    subtitle = "Year: 2001",
    x = "Deaths (per 1000 people)",
    y = "GDP (US$ Billions)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

deaths2021 <- ggplot(year2021, aes(deaths, gdp/1e9)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', se = TRUE, color = "darkred", size = 1) +
  labs(
    title = "Relationship between Deaths and GDP",
    subtitle = "Year: 2021",
    x = "Deaths (per 1000 people)",
    y = "GDP (US$ Billions)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

grid.arrange(deaths2001, deaths2021, nrow = 1)


#####################
### Net Migration ###
#####################

# Define bins for net migration
breaks <- seq(-2500000, 2000000, by = 500000)

# Bin 'net_migration' into intervals
data_migration <- data %>%
  mutate(net_migration_bin = cut(
    net_migration,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = paste(head(breaks, -1), tail(breaks, -1) - 1, sep = " - ")
  ))

# Plot boxplots of GDP for each net migration bin
ggplot(data_migration, aes(x = net_migration_bin, y = gdp/1e9)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "GDP Distribution by Net Migration",
    x = "Net Migration (per 1,000 persons)",
    y = "GDP (Billions USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


### Additional Net Migration Plot ###

migration2001 <- ggplot(year2001, aes(x = net_migration, y = gdp/1e9)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Net Migration vs GDP",
    subtitle = "Year 2001",
    x = "Net Migration (per 1,000)",
    y = "GDP (Billions USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

migration2021 <- ggplot(year2021, aes(x = net_migration, y = gdp/1e9)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Net Migration vs GDP",
    subtitle = "Year 2021",
    x = "Net Migration (per 1,000)",
    y = "GDP (Billions USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

grid.arrange(migration2001, migration2021, nrow = 1)



##############
### Forest ###
##############

data_forest <- data %>%
  mutate(forest_bin = cut(
    forest,
    breaks = seq(0, 100, by = 10),
    include.lowest = TRUE,
    right = FALSE,
    labels = paste(seq(0,90,10), seq(10,100,10), sep = "-")
  ))

# Plot jitter plot of GDP by forest bins
ggplot(data_forest, aes(x = forest_bin, y = gdp/1e9)) +
  geom_jitter(width = 0.2, alpha=0.6, color='steelblue') +
  labs(
    title = "GDP Distribution by Forest Coverage (% bins of 10%)",
    x = "Forest Coverage (%)",
    y = "GDP (Billions USD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Removing unnecessary variables after visualisations

data <- data %>% 
  select(-c(country, year, access_electricity, agricultural_land,
            births, deaths, forest, life_expectancy, net_migration,
            population, urban_population, region, rural_population, 
            agricultural_forestry_fishing, co2, gdp, co2_lag2,
            deaths_lag5, net_migration_lag2, population_lag2,
            log_co2_lag2, log_population_lag2
  ))

data<- na.omit(data)


###################
### Correlation ###
###################

# Descriptive Statistics following variable removal
describe(data)
corr_matrix <- cor(data, use = "complete.obs")
# Create your correlation plot
corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.9,
  diag = FALSE
)

#Add main title
title("Correlation Plot - Key Variables", line = 3, cex.main = 1.5, font.main = 2)



################################################################################
###############                                                  ###############
###############                  Modelling                       ###############
###############                                                  ###############
################################################################################

#######################
### Splitting Data  ###
#######################

# Setting the seed for reproducibility
set.seed(40153913)

# Splitting the data into train/test (80/20)
index <- createDataPartition(data$log_gdp, times = 1, p = 0.7, F)
train <- data[index, ]
test <- data[-index, ]

# Setting the cross-validation
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              verboseIter = F,
                              repeats = 3)


#################
###   LASSO   ###
#################

#tuning grid
tune_grid <- expand.grid(
  alpha = 1,
  lambda = 10^seq(5, -3, length = 100)
)

test_y <- test$log_gdp

set.seed(40153913)
# CV LASSO Model
lasso_caret <- caret::train(
  log_gdp ~ .,
  data = train,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = tune_grid
)

summary(lasso_caret$results)
lassocvresults <- lasso_caret$resample
summary(lassocvresults)
write_csv(lassocvresults, path = "LASSO_results.csv")

plot(lasso_caret)

#best hyperparameters and model summary
best_lambda <- lasso_caret$bestTune$lambda
print(lasso_caret)

# Make predictions
predictions <- predict(lasso_caret, newdata = test)

# performance metrics
rmse <- sqrt(mean((test_y - predictions)^2))
ss_total <- sum((test_y - mean(test_y))^2)
ss_residual <- sum((predictions - test_y)^2)
r_squared <- 1 - (ss_residual / ss_total)
mae <- mean(abs(test_y - predictions))
mape <- mean(abs((test_y - predictions) / test_y)) * 100

#Printing performance metrics
cat("LASSO (caret) RMSE:", rmse, "\n")
cat("LASSO (caret) R-squared:", r_squared, "\n")
cat("LASSO (caret) MAE:", mae, "\n")
cat("LASSO (caret) MAPE:", mape, "%\n")

# Get the coefficients at the best lambda
coef <- coef(lasso_caret$finalModel, s = best_lambda)

# Convert coefficients to a data frame, excluding the intercept
coef_df <- data.frame(
  variable = rownames(coef)[-1],
  coefficient = as.numeric(coef[-1])
)

#Filtering out variables with zero coefficients
coef_df <- coef_df[coef_df$coefficient != 0, ]

# Plot importance magnitudes
ggplot(coef_df, aes(x = reorder(variable, abs(coefficient)), y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Feature Importance from LASSO",
       x = "Variables",
       y = "Coefficient") +
  theme_minimal()


# Create the final formula
formula <- log_gdp ~ 
  access_electricity_lag3 + 
  agricultural_land_lag5 +
  log_deaths_lag5 + 
  forest_lag5 + 
  life_expectancy_lag2 +
  log_net_migration_lag2 + 
  urban_population_lag2



###############################
###    Linear Regression    ###
###############################

# Setting the LR CV
set.seed(40153913)
lm_cv <- caret::train(
  formula,
  data = train,
  method = 'lm',
  trControl = train_control
)

#Extracting best hyperparameters
best_lm <- lm_cv$bestTune

mlrcvresults <- lm_cv$resample
summary(mlrcvresults)

#Linear Regression using best hyperparameters
set.seed(40153913)
final_lm <- caret::train(
  formula,
  data = train,
  method = 'lm',
  trControl = trainControl(method = "none"),
  tuneGrid = best_lm
)

# Viewing the final model
final_lm <- lm_cv$finalModel
summary(final_lm)

# Checking assumptions of linear regression
vif(final_lm)
par(mfrow=c(2,2))
plot(final_lm)
par(mfrow=c(1,1))

# Making predictions on the best CV results
lm_cv_preds <- predict(final_lm, test)
postResample(lm_cv_preds, test$log_gdp)

mape_mlr <- mean(abs((test$log_gdp - lm_cv_preds) / test$log_gdp)) * 100
cat("MAPE:", mape_mlr, "%\n")



###############
###    DT   ###
###############

# Defining the tuning grid (hyperparameters)
tune_grid <- expand.grid(
  cp = seq(0.01, 0.1, by = 0.01)
)

# Cross-validation Decision Tree
set.seed(40153913)
dt_cv <- caret::train(
  formula,
  data = train,
  method = "rpart",
  trControl = train_control,
  tuneGrid = tune_grid
)

# Plot of complexity parameter
plot(dt_cv, main = "Decision Tree Complexity Parameter")

# View DT summary
dt_cv$results
print(dt_cv)

#CV results
dtcvresults <- dt_cv$resample
summary(dtcvresults)

# Identifying the best tuned results
dt_best <- dt_cv$bestTune

#Running decision tree with optimal parameters
set.seed(40153913)
dt_final <- caret::train(
  formula,
  data = train,
  method = "rpart",
  trControl = trainControl(method = "none"),
  tuneGrid = dt_best
)

# Predicting on test data
predictions_dt <- predict(dt_final, newdata = test)
postResample(predictions_dt, test$log_gdp)

# Calculating MAPE:
dt_mape <- mean(abs((test$log_gdp - predictions_dt) / test$log_gdp)) * 100
cat("MAPE:", dt_mape, "%\n")

# Viewing the final model
summary(dt_final)

# Plotting the final model
rpart.plot(dt_final$finalModel)

# Viewing the variable importance
importance <- varImp(dt_final)
print(importance)



#####################
### Random Forest ###
#####################

# Defining RF tuning grid
tune_grid_rf <- expand.grid(
  mtry = seq(2, 20, by = 2)
)

# Running Cross validation for RF
set.seed(40153913)
rf_cv <- caret::train(
  formula,
  data = train,
  method = "rf",
  trControl = train_control,
  tuneLength = 10,
  tuneGrid = tune_grid_rf,
  ntree = 500
)

# Isolating the best tuning metrics
best_mtry <- rf_cv$bestTune$mtry

#CV results
rfcvresults <- rf_cv$resample
summary(rfcvresults)

# Optimal number of randomly selected predictors
plot(rf_cv, main = "Optimal number of randomly selected predictors")

#viewing the results from the CV
print(rf_cv)

# Viewing final CV model
rf_cv$finalModel


#RF Model using the optimal tuning results
set.seed(40153913)
rf_final <- caret::train(
  formula,
  data = train,
  method = "rf",
  trControl = trainControl(method = "none"),
  tuneGrid = data.frame(mtry = best_mtry),
  ntree = 500
)

#Viewing the model
print(rf_final$finalModel)

# Number of Trees
plot(rf_final$finalModel, main = "Impact of Number of Trees on Error Rate")

# Predictions on test data
rf_preds_final <- predict(rf_final, newdata=test)
postResample(rf_preds_final, test$log_gdp)

# RF MAPE
rf_mape <- mean(abs((test$log_gdp - rf_preds_final) / test$log_gdp)) * 100
cat("MAPE:", rf_mape, "%\n")

# Variable importance
varImp(rf_final)



#####################
###      XGB      ###
#####################

# Defining the XGB grid to search
gbm_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6),
  eta = c(0.01, 0.1),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

# Cross validated XGB
set.seed(40153913)
xgb_cv <- caret::train(
  formula,
  data = train,
  method = "xgbTree",
  tuneGrid = gbm_grid,
  trControl = train_control,
  metric = "RMSE"
)

#CV results
xgbcvresults <- xgb_cv$resample
summary(xgbcvresults)

plot(xgb_cv, main = "XGB CV Iterations Plot")

# View the summary of the model
print(xgb_cv)

#Extract the best hyperparameters
xgb_best <- xgb_cv$bestTune

# XGB using optimal parameters
set.seed(40153913)
xgb_final <- caret::train(
  formula,
  data = train,
  method = "xgbTree",
  tuneGrid = xgb_best,
  trControl = trainControl(method = "none"),
  metric = "RMSE"
)

# XGB Variable Importance
varImp(xgb_final)

# make predictions on unseen data
preds_xgb <- predict(xgb_final, newdata = test)
postResample(preds_xgb, test$log_gdp)

# XGB MAPE
xgb_mape <- mean(abs((test$log_gdp - preds_xgb) / test$log_gdp)) * 100
cat("MAPE:", xgb_mape, "%\n")



#####################
###      SVR      ###
#####################

svm_grid <- expand.grid(
  sigma = c(0.01, 0.1, 1),
  C = c(1, 10, 100)
)

# Run Cross-validated SVR model
set.seed(40153913)
svm_cv <- caret::train(
  formula,
  data = train,
  method = "svmRadial",
  trControl = train_control,
  tuneGrid = svm_grid,
  metric = "RMSE"
)

#CV results
svmcvresults <- svm_cv$resample
summary(svmcvresults)

plot(svm_cv, main = "Sigma Impact on RMSE")

# Viewing which parameters achieved the best results
best_params <- svm_cv$bestTune
print(best_params)

# Viewing summaries of the SVM model
print(svm_cv)

# Final model being created
set.seed(40153913)
svm_final <- caret::train(
  formula,
  data = train,
  method = "svmRadial",
  trControl = trainControl(method = "none"),
  tuneGrid = best_params,
  metric = "RMSE"
)

# Making predictions on unseen data
predictions_svm <- predict(svm_final, newdata = test)
postResample(predictions_svm, test$log_gdp)

# SVM MAPE
svm_mape <- mean(abs((test$log_gdp - predictions_svm) / test$log_gdp)) * 100
cat("MAPE:", svm_mape, "%\n")



##############################
###          ANN           ###
##############################

# CV Neural Network
set.seed(40153913)
nn <- caret::train(formula,
                   data=train,
                   method='nnet',
                   linout = TRUE,
                   preProc = c("center", "scale"),
                   trControl = train_control,
                   trace=FALSE)


#CV results
nncvresults <- nn$resample
summary(nncvresults)


#Viewing the Neural Network
summary(nn)

# Plotting neural network
plot(nn, main = "How Number of Hidden Units Impact RMSE")
varImp(nn)
# Predictions on unseen data
preds_nn <- predict(nn, test)
postResample(preds_nn, test$log_gdp)

# MAPE
nn_mape <- mean(abs((test$log_gdp - preds_nn) / test$log_gdp)) * 100
cat("MAPE:", nn_mape, "%\n")



###############################
###         Hybrid          ###
###############################

algorList <- c('rf', 'xgbTree', 'svmRadial')

trainControl_hy <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  savePredictions = "final"
)

set.seed(40153913)
models <- caretList(
  formula,
  data = train,
  trControl = trainControl_hy,
  methodList = algorList
)
summary(models)

# Collect resampling results
hmresults <- resamples(models)

#CV results
summary(hmresults)
bwplot(hmresults, scales = list(x = list(relation = "free"), y = list(relation = "free")))

# Set seed for reproducibility
set.seed(40153913)

# stacking control
stackControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  savePredictions = "final"
)

#Ensemble with glm
stack.glm <- caretStack(
  models,
  method = "glm",
  trControl = stackControl
)

#CV results
hmcvresults <- stack.glm$error
summary(hmcvresults)

# Print the stack model
print(stack.glm)

# Predictions
stack_preds <- predict(stack.glm, newdata = test)
postResample(stack_preds, test$log_gdp)

# Hybrid MAPE
preds <- stack_preds$pred
hy_mape <- mean(abs((test$log_gdp - preds) / test$log_gdp)) * 100
cat("MAPE:", hy_mape, "%\n")


#################
###    XAI    ###
#################

# Subset relevant features
features <- c("access_electricity_lag3", 
              "agricultural_land_lag5", 
              "log_deaths_lag5",
              "forest_lag5",
              "life_expectancy_lag2",
              "log_net_migration_lag2", 
              "urban_population_lag2")

X <- data[, features]

# Define predictor object
predictor <- Predictor$new(
  model = svm_final, #####  best performer
  data = X,
  y = data$log_gdp
)

#Permutation Variable Importance
imp = FeatureImp$new(predictor, loss = "mae")
plot(imp, title = "Variable Importance for SVR Model")
imp$results

### Accumulated Local Effects
#Life Expectancy ALE
ale_electricity <- FeatureEffect$new(predictor, feature = "access_electricity_lag3")
ale_electricity$plot()


#Life Expectancy ALE
ale_life_expectancy <- FeatureEffect$new(predictor, feature = "life_expectancy_lag2")
ale_life_expectancy$plot()

#Life Expectancy ALE
ale_agricultural <- FeatureEffect$new(predictor, feature = "agricultural_land_lag5")
ale_agricultural$plot()

#Deaths ALE
ale_deaths <- FeatureEffect$new(predictor, feature = "log_deaths_lag5")
ale_deaths$plot()

#Forest ALE
ale_forest <- FeatureEffect$new(predictor, feature = "forest_lag5")
ale_forest$plot()

#Net Migration ALE
ale_net_migration <- FeatureEffect$new(predictor, feature = "log_net_migration_lag2")
ale_net_migration$plot()

#Urban Population ALE
ale_urban_population <- FeatureEffect$new(predictor, feature = "urban_population_lag2")
ale_urban_population$plot()


