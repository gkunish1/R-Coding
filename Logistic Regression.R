library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(stringr)
library(forcats)
library(lmtest)
library(sandwich)

# expiriment with residential units
# Try to use features with numerics -> causes more variation
#


df_2016 <- read.csv('2016_brooklyn.csv', skip = 4)
df_2017 <- read.csv('2017_brooklyn.csv', skip = 4)
df_2018 <- read.csv('2018_brooklyn.csv', skip = 4)
df_2019 <- read.csv('2019_brooklyn.csv', skip = 4)
df_2020 <- read.csv('2020_brooklyn.csv', skip = 6)



colnames(df_2016) <-c("BOROUGH","NEIGHBORHOOD","BUILDING_CLASS_CATEGORY","TAX_CLASS_AT_PRESENT","BLOCK","LOT","EASE-MENT","BUILDING_CLASS_AT_PRESENT","ADDRESS","APARTMENT_NUMBER","ZIP_CODE","RESIDENTIAL_UNITS","COMMERCIAL_UNITS","TOTAL_UNITS","LAND_SQUARE_FEET","GROSS_SQUARE_FEET","YEAR_BUILT","TAX_CLASS_AT_TIME_OF_SALE","BUILDING_CLASS_AT_TIME_OF_SALE","SALE_PRICE","SALE_DATE")
colnames(df_2017) <-c("BOROUGH","NEIGHBORHOOD","BUILDING_CLASS_CATEGORY","TAX_CLASS_AT_PRESENT","BLOCK","LOT","EASE-MENT","BUILDING_CLASS_AT_PRESENT","ADDRESS","APARTMENT_NUMBER","ZIP_CODE","RESIDENTIAL_UNITS","COMMERCIAL_UNITS","TOTAL_UNITS","LAND_SQUARE_FEET","GROSS_SQUARE_FEET","YEAR_BUILT","TAX_CLASS_AT_TIME_OF_SALE","BUILDING_CLASS_AT_TIME_OF_SALE","SALE_PRICE","SALE_DATE")
colnames(df_2018) <-c("BOROUGH","NEIGHBORHOOD","BUILDING_CLASS_CATEGORY","TAX_CLASS_AT_PRESENT","BLOCK","LOT","EASE-MENT","BUILDING_CLASS_AT_PRESENT","ADDRESS","APARTMENT_NUMBER","ZIP_CODE","RESIDENTIAL_UNITS","COMMERCIAL_UNITS","TOTAL_UNITS","LAND_SQUARE_FEET","GROSS_SQUARE_FEET","YEAR_BUILT","TAX_CLASS_AT_TIME_OF_SALE","BUILDING_CLASS_AT_TIME_OF_SALE","SALE_PRICE","SALE_DATE")
colnames(df_2019) <-c("BOROUGH","NEIGHBORHOOD","BUILDING_CLASS_CATEGORY","TAX_CLASS_AT_PRESENT","BLOCK","LOT","EASE-MENT","BUILDING_CLASS_AT_PRESENT","ADDRESS","APARTMENT_NUMBER","ZIP_CODE","RESIDENTIAL_UNITS","COMMERCIAL_UNITS","TOTAL_UNITS","LAND_SQUARE_FEET","GROSS_SQUARE_FEET","YEAR_BUILT","TAX_CLASS_AT_TIME_OF_SALE","BUILDING_CLASS_AT_TIME_OF_SALE","SALE_PRICE","SALE_DATE")
colnames(df_2020) <-c("BOROUGH","NEIGHBORHOOD","BUILDING_CLASS_CATEGORY","TAX_CLASS_AT_PRESENT","BLOCK","LOT","EASE-MENT","BUILDING_CLASS_AT_PRESENT","ADDRESS","APARTMENT_NUMBER","ZIP_CODE","RESIDENTIAL_UNITS","COMMERCIAL_UNITS","TOTAL_UNITS","LAND_SQUARE_FEET","GROSS_SQUARE_FEET","YEAR_BUILT","TAX_CLASS_AT_TIME_OF_SALE","BUILDING_CLASS_AT_TIME_OF_SALE","SALE_PRICE","SALE_DATE")


df <- rbind(df_2016,df_2017,df_2018,df_2019,df_2020)

df <- df %>%
  mutate(NEIGHBORHOOD = trimws(NEIGHBORHOOD),
         BUILDING_CLASS_CATEGORY = trimws(BUILDING_CLASS_CATEGORY),
         SALE_PRICE = trimws(SALE_PRICE),
         GROSS_SQUARE_FEET = trimws(GROSS_SQUARE_FEET),
         LAND_SQUARE_FEET = trimws(LAND_SQUARE_FEET),
         BUILDING_CLASS_AT_TIME_OF_SALE = trimws(BUILDING_CLASS_AT_TIME_OF_SALE))


df <- df %>%
  mutate(SALE_PRICE = as.numeric(gsub('\\D','',SALE_PRICE)),
         GROSS_SQUARE_FEET = as.numeric(gsub('\\D','', GROSS_SQUARE_FEET)),
         LAND_SQUARE_FEET = as.numeric(gsub('\\D','', LAND_SQUARE_FEET)))



df <- df %>%
  mutate(
    across(c(1:8, 11, 18,19), as.factor),
    across(c(12, 13, 14), as.integer),
    across(21, ~ as.Date(., format = "%m/%d/%Y")))


##building class at sale A or R && total units & residential units == 1
df_filtered <- df %>%
  filter(
    str_starts(BUILDING_CLASS_AT_TIME_OF_SALE, "A") |
      str_starts(BUILDING_CLASS_AT_TIME_OF_SALE, "R"),
    TOTAL_UNITS == 1,
    RESIDENTIAL_UNITS == 1
  )

##gross square footage is more than 0 and sale price is non-missing
df_filtered <- df_filtered %>%
  filter(
    GROSS_SQUARE_FEET > 0,
    !is.na(SALE_PRICE)
  )


df_filtered <- df_filtered %>%
  mutate(
    across(c(12, 13, 14), as.factor))

quantile(df_filtered$SALE_PRICE, probs = seq(0, 1, 0.01))

##dropping  year built =< 0, sales price =< 0
df_filtered <- df_filtered%>%
  filter(SALE_PRICE >100000)
df_filtered <- df_filtered %>%
  filter(SALE_PRICE <5000000)
df_filtered <- df_filtered %>%
  filter(YEAR_BUILT > 0)


##zip to region
NORTH<- c('22','11','51','01','31','17','05','38','16','21','37','06')
SOUTH<- c('14','23','29','34','24','35')
EAST<- c('25','13','33','26','03','12','07','08','10','36','39')
WEST<- c('09','20','32','15','18','19','28','04','30')
WEST <- as.factor(WEST)
EAST <- as.factor(EAST)
SOUTH <- as.factor(SOUTH)
NORTH <- as.factor(NORTH)

df_filtered <- df_filtered %>%
  mutate(ZIP_CODE = as.factor(substr(as.character(ZIP_CODE), nchar(as.character(ZIP_CODE)) - 1, nchar(as.character(ZIP_CODE)))))

df_filtered <- df_filtered %>%
  mutate(REGION = case_when(
    ZIP_CODE %in% NORTH ~ "NORTH",
    ZIP_CODE %in% SOUTH ~ "SOUTH",
    ZIP_CODE %in% EAST ~ "EAST",
    TRUE ~ "WEST"
  ))


df_filtered <- df_filtered %>%
  mutate(YEAR_BUILT = as.numeric(as.character(YEAR_BUILT))) %>%
  mutate(
    YEAR_BUILT_BIN = case_when(
      YEAR_BUILT < 1920 ~ "Pre-1920",
      YEAR_BUILT >= 1920 & YEAR_BUILT < 1930 ~ "1920s",
      YEAR_BUILT >= 1930 & YEAR_BUILT < 1960 ~ "1930s–1950s",
      YEAR_BUILT >= 1960 & YEAR_BUILT < 1990 ~ "1960s–1980s",
      YEAR_BUILT >= 1990 & YEAR_BUILT < 2010 ~ "1990s–2000s",
      YEAR_BUILT >= 2010 ~ "2010s"
    ),
    YEAR_BUILT_BIN = factor(YEAR_BUILT_BIN, levels = c(
      "Pre-1920", "1920s", "1930s–1950s", "1960s–1980s", "1990s–2000s", "2010s"
    ))
  ) %>%
  select(-YEAR_BUILT)




lm <- lm(log(SALE_PRICE) ~ (YEAR_BUILT_BIN + LAND_SQUARE_FEET)^2
          + (YEAR_BUILT_BIN + GROSS_SQUARE_FEET)^2
          + (REGION + LAND_SQUARE_FEET)^2
          + (REGION + GROSS_SQUARE_FEET)^2
          + LAND_SQUARE_FEET + GROSS_SQUARE_FEET, data = df_filtered)
summary(lm)


preds <- exp(predict(lm))
actuals <- df_filtered$SALE_PRICE

rmse <- sqrt(mean((preds - actuals)^2))
rmse

df_filtered1 <- df_filtered %>%
   mutate(LOT_COVERAGE = GROSS_SQUARE_FEET / LAND_SQUARE_FEET)

df_filtered1 <- df_filtered1 %>%
   mutate(LOG_LAND_SQFT = log(LAND_SQUARE_FEET))

df_filtered1 <- df_filtered1 %>%
   mutate(LOG_GROSS_SQFT = log(GROSS_SQUARE_FEET))

df_filtered_new <- df_filtered1 %>%
  select(SALE_PRICE, LOT_COVERAGE, REGION, LOG_LAND_SQFT, LOG_GROSS_SQFT, YEAR_BUILT_BIN, SALE_DATE)

colSums(is.na(df_filtered_new))
sum(!is.finite(df_filtered_new$LOG_LAND_SQFT))
sum(!is.finite(df_filtered_new$LOG_GROSS_SQFT))
sum(!is.finite(df_filtered_new$LOG_LOT_COVERAGE))
df_filtered_new <- df_filtered_new %>%
   filter(
     is.finite(LOG_LAND_SQFT)
   )


lm1 <- lm(log(SALE_PRICE) ~ (YEAR_BUILT_BIN + LOT_COVERAGE)^2 
          + (YEAR_BUILT_BIN + LOG_LAND_SQFT)^2
          + (YEAR_BUILT_BIN + LOG_GROSS_SQFT)^2
          + (REGION + LOT_COVERAGE)^2
          + (REGION + LOG_LAND_SQFT)^2
          + (REGION + LOG_GROSS_SQFT)^2
          + LOT_COVERAGE + LOG_LAND_SQFT + LOG_GROSS_SQFT, data = df_filtered_new)
summary(lm1)

preds <- exp(predict(lm1))
actuals <- df_filtered_new$SALE_PRICE

rmse <- sqrt(mean((preds - actuals)^2))
rmse

qqnorm(lm1$residuals)
qqline(lm1$residuals)

bptest(lm1)
dwtest(lm1)

lm2 <- lm(SALE_PRICE ~ (YEAR_BUILT_BIN + LOT_COVERAGE)^2 
          + (YEAR_BUILT_BIN + LOG_LAND_SQFT)^2
          + (YEAR_BUILT_BIN + LOG_GROSS_SQFT)^2
          + (REGION + LOT_COVERAGE)^2
          + (REGION + LOG_LAND_SQFT)^2
          + (REGION + LOG_GROSS_SQFT)^2
          + LOT_COVERAGE + LOG_LAND_SQFT + LOG_GROSS_SQFT, data = df_filtered_new)
summary(lm2)

preds <- predict(lm2)
actuals <- df_filtered_new$SALE_PRICE

rmse <- sqrt(mean((preds - actuals)^2))
rmse

qqnorm(lm2$residuals)
qqline(lm2$residuals)

bptest(lm2)
dwtest(lm2)

########## Data with only Q3 and Q4 ##########
df_filtered_new <- df_filtered_new %>%
  mutate(
    LOT_COVERAGE = as.numeric(LOT_COVERAGE),
    LOG_LAND_SQFT = as.numeric(LOG_LAND_SQFT),
    LOG_GROSS_SQFT = as.numeric(LOG_GROSS_SQFT)
  )

df_filtered_2020 <- df_filtered_new %>% mutate(year = as.numeric(format(SALE_DATE, "%Y"))) %>% filter(year == 20) %>% 
  mutate(quarter = quarter(SALE_DATE)) %>% mutate(sales_qt_2020q4 = case_when(quarter == 4 ~ 1, 
                                                                              quarter == 3 ~ 0)) %>% filter(quarter == 3 | quarter == 4)
# Create lag of 1 period (quarter)


########## Run Models using 2020 Q3 and Q4 Data ##########

### Original Model applied to 2020 Q3 and 2020 Q4 Data ###
model <-  lm(SALE_PRICE ~ (YEAR_BUILT_BIN + LOT_COVERAGE)^2 
             + (YEAR_BUILT_BIN + LOG_LAND_SQFT)^2
             + (YEAR_BUILT_BIN + LOG_GROSS_SQFT)^2
             + (REGION + LOT_COVERAGE)^2
             + (REGION + LOG_LAND_SQFT)^2
             + (REGION + LOG_GROSS_SQFT)^2
             + LOT_COVERAGE + LOG_LAND_SQFT + LOG_GROSS_SQFT,data = df_filtered_2020)

### New Model (CHANGES: LOG SALES PRICE AND ADDED BINARY VARIABLE FOR Q3 VS Q4)

model_with_binary <- lm(log(SALE_PRICE) ~ (YEAR_BUILT_BIN + LOT_COVERAGE)^2 
                        + (YEAR_BUILT_BIN + LOG_LAND_SQFT)^2
                        + (YEAR_BUILT_BIN + LOG_GROSS_SQFT)^2
                        + (REGION + LOT_COVERAGE)^2
                        + (REGION + LOG_LAND_SQFT)^2
                        + (REGION + LOG_GROSS_SQFT)^2
                        + LOT_COVERAGE + LOG_LAND_SQFT + LOG_GROSS_SQFT + sales_qt_2020q4,data = df_filtered_2020)

summary(model_with_binary)

###### Checking for Normal Errors ##########

residuals_model <- resid(model_with_binary)
# Standardize residuals
residuals_std <- (residuals_model - mean(residuals_model)) / sd(residuals_model)

# Kolmogorov–Smirnov test against standard normal (fails tests but looks normal)
ks.test(residuals_std, "pnorm")
shapiro.test(residuals_model)

hist(residuals_std, breaks = 30, col = "steelblue", main = "Histogram of Residuals", xlab = "Standardized Residuals")

# Q-Q plot to assess normality
qqnorm(residuals_std, main = "Q-Q Plot")
qqline(residuals_std, col = "red", lwd = 2)

# NOTES: Even though the model failed the KS test, we still observe relatively normal residuals based on the histogram and Q-Q plots. 



########## Checking for Autocorrelation ##########
dwtest(model_with_binary)
acf(residuals_std)


##Notes: No autocorrelation after adding binary variable for quarter between Q3 and Q4 


########## Heteroskedasicty ##########
bptest(model_with_binary)

plot(fitted(model_with_binary), residuals(model_with_binary), 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  

#Notes: Failed BP test, but we don't observe a clear pattern when looked at fitted values vs residuals so we safely assume homoscedastic errors. 



##### Confidence Interval and Interpretation of Model #####
summary(model_with_binary)
confint(model_with_binary)

mean_sales_price_q3 <- df_filtered_2020 %>%
  filter(quarter == 3) %>%
  summarise(mean_sales_price = mean(SALE_PRICE, na.rm = TRUE))

mean_sales_price_q3

#### Notes ###
#Properties sold in Q4 (as compared to Q3) have a 7.42% higher sales price on average, holding other factors constant.
#Dollar Change: The average home price in 2020 q3 was 980,039.4. This means that the sales price in 2020 Q4 is expected to be on average $72,718.92 higher. 



########## Run Tranformed Model on Entire Dataset ##########
df_filtered_new <-  df_filtered_new %>% mutate(
  year = year(SALE_DATE),
  quarter = quarter(SALE_DATE)
) %>%   mutate(year = ifelse(year < 100, year + 2000, year)) %>% mutate(
  year_quarter = paste0("Q", quarter, "_", year)
)


model_with_fixed <- lm(log(SALE_PRICE) ~ (YEAR_BUILT_BIN + LOT_COVERAGE)^2 
                       + (YEAR_BUILT_BIN + LOG_LAND_SQFT)^2
                       + (YEAR_BUILT_BIN + LOG_GROSS_SQFT)^2
                       + (REGION + LOT_COVERAGE)^2
                       + (REGION + LOG_LAND_SQFT)^2
                       + (REGION + LOG_GROSS_SQFT)^2
                       + LOT_COVERAGE + LOG_LAND_SQFT + LOG_GROSS_SQFT + factor(year_quarter),data = df_filtered_new)

summary(model_with_fixed)

preds <- exp(predict(model_with_fixed))
actuals <- df_filtered_new$SALE_PRICE

rmse <- sqrt(mean((preds - actuals)^2))
rmse



########## Distribution of Sales Prices ##########
df_filtered_2020$quarter <- factor(df_filtered_2020$quarter, levels = c(4, 3))

ggplot(df_filtered_2020, aes(x = SALE_PRICE, fill = quarter)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),position = "identity", alpha = 0.6, bins = 70) +
  scale_x_continuous(labels = scales::dollar_format()) +   scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Raw Sale Prices: Q3 vs Q4 2020",
       x = "Sale Price",
       y = "Percent") +
  theme_minimal() +
  scale_fill_manual(values = c("3" = "indianred", "4" = "steelblue"), labels = c("3" = "Q3 2020", "4" = "Q4 2020"), name = "Quarter")



########## Residual Plots ###########

df_filtered_2020$residual <- resid(model)
df_filtered_2020$residual_with_binary <- resid(model_with_binary)
df_filtered_2020$quarter <- factor(df_filtered_2020$quarter, levels = c(4, 3))

### Residual Plot of Original Modeel's Residuals ###
ggplot(df_filtered_2020, aes(x = residual, fill = factor(quarter))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 50,
                 aes(y = after_stat(count / sum(count)))) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("indianred", "steelblue"), labels = c("3" = "Q3 2020", "4" = "Q4 2020"), name = "Quarter" ) +
  labs(
    title = "Residuals Q3 vs Q4 2020", 
    x = "Residual (Without Quarter Binary)",
    y = "Percent of Sales",
    fill = "Quarter"
  ) +
  theme_minimal()


### Residual Plot of nTransformed Model's Residuals ###

ggplot(df_filtered_2020, aes(x = residual_with_binary, fill = factor(quarter))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 50,
                 aes(y = after_stat(count / sum(count)))) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("indianred", "steelblue"),  labels = c("3" = "Q3 2020", "4" = "Q4 2020"), name = "Quarter" ) +
  labs(
    title = "Residuals: Q3 vs Q4 2020",
    x = "Residual (With Quarter Binary)",
    y = "Percent of Sales",
    fill = "Quarter"
  ) +
  theme_minimal() 


########## Evaluation of Full Model ##########
df_filtered_new <-  df_filtered_new %>% mutate(
  year = year(SALE_DATE),
  quarter = quarter(SALE_DATE)
) %>%   mutate(year = ifelse(year < 100, year + 2000, year)) %>% mutate(
  year_quarter = paste0("Q", quarter, "_", year)
)


model_with_fixed <- lm(log(SALE_PRICE) ~ (YEAR_BUILT_BIN + LOT_COVERAGE)^2 
                       + (YEAR_BUILT_BIN + LOG_LAND_SQFT)^2
                       + (YEAR_BUILT_BIN + LOG_GROSS_SQFT)^2
                       + (REGION + LOT_COVERAGE)^2
                       + (REGION + LOG_LAND_SQFT)^2
                       + (REGION + LOG_GROSS_SQFT)^2
                       + LOT_COVERAGE + LOG_LAND_SQFT + LOG_GROSS_SQFT + factor(year_quarter),data = df_filtered_new)

summary(model_with_fixed)
length(coef(model_with_fixed))


###### Checking for Normal Errors ##########

residuals_model <- resid(model_with_fixed)
# Standardize residuals
residuals_std <- (residuals_model - mean(residuals_model)) / sd(residuals_model)

# Kolmogorov–Smirnov test against standard normal (fails tests but looks normal)
ks.test(residuals_std, "pnorm")
shapiro.test(residuals_model)

hist(residuals_std, breaks = 30, col = "steelblue", main = "Histogram of Residuals", xlab = "Standardized Residuals")

# Q-Q plot to assess normality
qqnorm(residuals_std, main = "Q-Q Plot")
qqline(residuals_std, col = "red", lwd = 2)

# NOTES: Even though the model failed the KS test, we still observe relatively normal residuals based on the histogram and Q-Q plots. 



########## Checking for Autocorrelation ##########
dwtest(model_with_fixed)


##Notes: Autocorrelation but could be fixed if we had a cross-sectional panel that would allow us to add lagged sales prices to correct autocorrelation



########## Heteroskedasicty ##########
bptest(model_with_fixed)

plot(fitted(model_with_fixed), residuals(model_with_fixed), 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  

#Notes: Failed BP test, but we don't observe a clear pattern when looked at fitted values vs residuals so we safely assume homoscedastic errors. 

