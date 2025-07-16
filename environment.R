# Load libraries
library(tidyverse)
library(lubridate)
library(caret)

# Load and clean dataset
df <- data %>%
  mutate(across(c(TEMP, PRCP, HMDT, WND_SPD, ATM_PRESS), as.numeric)) %>%
  mutate(across(everything(), ~ na_if(., -999))) %>%
  drop_na()

# Create Date and Season columns
df <- df %>%
  mutate(Date = make_date(YEAR, MO, DY),
         Season = case_when(
           MO %in% c(12, 1, 2) ~ "Winter",
           MO %in% c(3, 4, 5) ~ "Spring",
           MO %in% c(6, 7, 8) ~ "Summer",
           MO %in% c(9, 10, 11) ~ "Fall"
         ))

# Aggregate hourly data to daily
daily_df <- df %>%
  group_by(Date, Season) %>%
  summarise(
    TEMP = mean(TEMP),
    PRCP = sum(PRCP),
    HMDT = mean(HMDT),
    WND_SPD = mean(WND_SPD),
    ATM_PRESS = mean(ATM_PRESS),
    .groups = "drop"
  )

# Split into train/test
set.seed(323)
train_index <- createDataPartition(daily_df$TEMP, p = 0.8, list = FALSE)
train <- daily_df[train_index, ]
test <- daily_df[-train_index, ]

# Simple Linear Regression
model1 <- lm(TEMP ~ HMDT, data = train)
summary(model1)

# Multiple Linear Regression
model2 <- lm(TEMP ~ HMDT + PRCP + WND_SPD + ATM_PRESS, data = train)
summary(model2)

# Predict and calculate RMSE on test set
preds <- predict(model2, newdata = test)
rmse <- sqrt(mean((test$TEMP - preds)^2))
cat("RMSE on test set:", rmse, "\n")

# Seasonal Regression Models
season_models <- daily_df %>%
  group_split(Season) %>%
  set_names(unique(daily_df$Season)) %>%
  map(~ lm(TEMP ~ HMDT + PRCP + WND_SPD + ATM_PRESS, data = .x))

# Show model summaries for each season
season_summaries <- map(season_models, summary)
season_summaries$Winter
season_summaries$Spring
season_summaries$Summer
season_summaries$Fall

