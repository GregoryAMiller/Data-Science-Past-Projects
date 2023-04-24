#######################################   Libraries Needed    ######################################################
library(tidyverse)
library(class)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
# Change the directory where the csv files can be found
setwd("C:/Users/Grego/Documents/490 Class/UsEconCSVs")

#######################################   Importing Yearly Data, Formatting Yearly Data Section    ######################################################

#######################################   Yearly CPI Data, Cleaning CPI Data    ######################################################
cpi_data <- read_csv("cpi.csv")
cpi_data %>% view()
cpi_data_cleaning <- cpi_data %>% separate(Date, sep="-", into = c("year", "month", "day")) %>% view()
cpi_data_final <- select(cpi_data_cleaning, -c(2,3)) %>% view()
cpi_data_final %>% rename(cpi = Rate) %>% view()
cpi_data_final <- cpi_data_final %>% group_by(year) %>% summarise(across(Rate, mean)) %>% view()
cpi_data_final <- cpi_data_final %>% rename(cpi = Rate) %>% view()
#######################################   Yearly employment Data, Date Format (1941-2010)    ######################################################
employment_data <- read_csv("employment.csv")
employment_data %>% view()
#######################################   Yearly GDP Data, Date Format (1930-2015)    ######################################################
gdp_data <- read_csv("gdp_year.csv")
gdp_data %>% view()
#######################################   Yearly Housing Price Data, Data Cleaning, Date Format (1/1/1987 every month till 12/1/2015)    ######################################################
house_price_data <- read_csv("housePrice_cities.csv")
house_price_data %>% view()
# split year-month-day to their own columns so we can mutate off of year
house_price_cond <- house_price_data %>% separate(Date, sep="-", into = c("year", "month", "day")) %>% view()
# removing cities that dont have values starting from the beggining of data
house_price_cond <- select(house_price_cond, -c(4,12,15,16,22,23,25)) %>% view()
# remove NA values
house_price_data_no_na <- house_price_cond %>% drop_na() %>% view() # dallas , and a few others have a lot of na values and only start giving real values around 2000 should they be dropped?
# summarise by year and take the mean of that sum
house_price_final <- house_price_data_no_na %>% group_by(year) %>% summarise(across(3:18, mean)) %>% view()
#######################################   Yearly Bond Yield Data, Data Cleaning    ######################################################
bond_yield_data <- read_csv("bondyield_monthly.csv")
bond_yield_data %>% view()
bond_yield_final_data <- bond_yield_data %>% separate(Date, sep="-", into = c("year", "month", "day")) %>% group_by(year) %>% summarise(across(Rate, mean)) %>% view()
bond_yield_final_data <- bond_yield_final_data %>% rename(bondYieldRate = Rate) %>% view()
#######################################   Yearly Household Income Data, Data Cleaning, Date Format (1967 till 2016)    ######################################################
house_income_data <- read_csv("household-income-us-historical.csv")
house_income_data %>% view()
#######################################   Yearly Household Income Data, Data Cleaning, Date Format (2007-01-31 till 2015-02-28)    ######################################################
inv_flow_data <- read_csv("inv-flow-of-funds-monthly.csv") # will need to change to yearly bc most other data is in yearly format
inv_flow_data %>% view()
#######################################   Yearly Education Budget Data, Date Format (1976 till 2016)    ######################################################
education_data <- read_csv("usa-education-budget-analysis-data.csv")
education_data %>% view()

#######################################   Join Tables Created Above Into One Table   ######################################################
join1 <- employment_data %>% inner_join(gdp_data, by=c(year="date")) %>% view()
join2 <- join1 %>% inner_join(education_data, by=c(year="YEAR")) %>% view()
join3 <- join2 %>% inner_join(house_income_data, by=c(year="Year")) %>% view()
join3$year <- as.character(join3$year) 
join3 %>% view()
join4 <- join3 %>% inner_join(house_price_final) %>% view()
join5 <- join4 %>% inner_join(bond_yield_final_data) %>% view()


#######################################   Save Table Above Into CSV File    ######################################################
write.csv(join5, "C:/Users/Grego/Desktop/usEconCSVs/temp_final_data.csv")

temp_data <- read_csv("temp_final_data.csv")
temp_data %>% view()

temp_data <- select(temp_data, -13) %>% view()

write.csv(temp_data, "C:/Users/Grego/Desktop/usEconCSVs/temp_final_data_nona.csv")

# Test models to see if everything is working
lm1 <- lm(formula = GDP~unemployed_percent + population, data = temp_data)
summary(lm1)
lm2 <- lm(formula = GDP~unemployed_percent + population + Lowest +Second + Third + Fourth + RATIO, data = temp_data)
summary(lm2)
lm3 <- lm(formula = GDP~Rate , data = temp_data)
summary(lm3)

# additional clearning and joining of the tables for midterm point of class
real_gdp_data <- read_csv("GDP.csv") %>% view()
datathis <- real_gdp_data %>% separate(DATE, sep="-", into = c("year", "month", "day")) %>% view()
datathis2 <- select(datathis, -c(2,3)) %>% view()
datathis2$year <- as.character(datathis2$year) 
employment_data$year <- as.character(employment_data$year) 
employ_gdp_data <- employment_data %>% inner_join(datathis2) %>% view()
employ_gdp_data <- select(employ_gdp_data, -c(12)) %>% view()
gdp_data$date <- as.character(gdp_data$date) 
employ_gdp_data$year <- as.character(employ_gdp_data$year) 
employ_gdp_data_more <- employ_gdp_data %>% inner_join(gdp_data, by=c(year="date")) %>% view()
final_employ_gdp_data <- select(employ_gdp_data_more, -c(12)) %>% view()
maybe_final_data <- final_employ_gdp_data %>% inner_join(cpi_data_final) %>% view()
maybe_final_data2 <- maybe_final_data %>% inner_join(bond_yield_final_data) %>% view()

write.csv(maybe_final_data2, "C:/Users/Grego/Desktop/usEconCSVs/midtermData.csv")

#######################################   Rename Variables Creating Issues With Lm()    ######################################################
econ_data <- read_csv("midtermData.csv")
econ_data %>% view()
econ_data <- select(econ_data, -c(1)) %>% view()
# rename some variable names since they are giving errors in LM()
econ_data <- econ_data %>% rename(levelchained = `level-chained`) %>% view()
econ_data <- econ_data %>% rename(levelcurr = `level-current`) %>% view()
econ_data <- econ_data %>% rename(changecurr = `change-current`) %>% view()
econ_data <- econ_data %>% rename(changechained = `change-chained`) %>% view()
econ_data <- econ_data %>% rename(bondY = bondYieldRate) %>% view()
econ_data %>% view()
write.csv(econ_data, "C:/Users/Grego/Desktop/usEconCSVs/midtermData.csv")

#######################################  Creating Modeling Using Data From Above    ######################################################

econ_data <- read_csv("midtermData.csv")
econ_data %>% view()
econ_data <- select(econ_data, -c(1)) %>% view()

year_data <- econ_data %>% select(!'year') %>% view()

m <- cor(year_data)

corrplot(m, method='color')
#######################################  Linear Model 1 Yearly, Summary, Predictions, Residuals    ######################################################

lm1 <- lm(formula = levelchained~unemployed_percent + population, data = econ_data)
summary(lm1)
plot(lm1)

lm2_qrt_test_data <- qrt_test_data %>% mutate(ypred = predict(lm2_qrt_train, newdata = qrt_test_data), residuals = perc_ch_GDP - ypred) %>% view()
write.csv(lm2_qrt_test_data, "C:/Users/Grego/Documents/490 Class/UsEconCSVs/2ypred.csv")

lm1_year_data <- econ_data %>% mutate(ypred = predict(lm1, newdata = econ_data), residuals = levelchained - ypred) %>% view()

# Cross validation correlation: When squared can be similarly to a R2 value

lm1_year_corr <- lm1_year_data %>% summarize(cor = cor(levelchained, ypred)) %>% mutate(R2 = cor^2) %>% view()


#######################################  Linear Model 2 Yearly, Summary, Predictions, Residuals    ######################################################

lm2 <- lm(formula = levelchained~unemployed_percent + cpi + bondY, data = econ_data)
summary(lm2)

lm2_year_data <- econ_data %>% mutate(ypred = predict(lm1, newdata = econ_data), residuals = levelchained - ypred) %>% view()


#######################################  Linear Model 3 Yearly, Summary, Predictions, Residuals    ######################################################

lm3 <- lm(formula = levelchained~cpi, data = econ_data)
summary(lm3)

lm3_year_data <- econ_data %>% mutate(ypred = predict(lm1, newdata = econ_data), residuals = levelchained - ypred) %>% view()

#######################################  Linear Model 4 Yearly, Summary, Predictions, Residuals    ######################################################

lm4 <- lm(formula = levelchained~unemployed_percent + population + bondY + cpi + labor_force, data = econ_data)
summary(lm4)

lm4_year_data <- econ_data %>% mutate(ypred = predict(lm1, newdata = econ_data), residuals = levelchained - ypred) %>% view()

#######################################  Linear Model 5 Yearly, Summary, Predictions, Residuals    ######################################################

lm5 <- lm(formula = levelchained~unemployed_percent + population + bondY + cpi + labor_force + changecurr, data = econ_data)
summary(lm5)

lm5_year_data <- econ_data %>% mutate(ypred = predict(lm1, newdata = econ_data), residuals = levelchained - ypred) %>% view()

#######################################  Linear Model 6 Yearly, Summary, Predictions, Residuals    ######################################################

lm6 <- lm(formula = levelchained~unemployed_percent + bondY + changechained, data = econ_data)
summary(lm6)

lm6_year_data <- econ_data %>% mutate(ypred = predict(lm1, newdata = econ_data), residuals = levelchained - ypred) %>% view()

###########################################################
lm1 <- lm(formula = levelcurr~unemployed_percent + population, data = econ_data)
summary(lm1)

lm2 <- lm(formula = levelcurr~unemployed_percent + cpi + bondY, data = econ_data)
summary(lm2)

lm3 <- lm(formula = levelcurr~cpi, data = econ_data)
summary(lm3)

lm4 <- lm(formula = levelcurr~unemployed_percent + population + bondY + cpi + labor_force, data = econ_data)
summary(lm4)
###########################################################
lm1 <- lm(formula = changecurr~unemployed_percent + population, data = econ_data)
summary(lm1)

lm2 <- lm(formula = changecurr~unemployed_percent + cpi + bondY, data = econ_data)
summary(lm2)

lm3 <- lm(formula = changecurr~cpi, data = econ_data)
summary(lm3)

lm4 <- lm(formula = changecurr~unemployed_percent + population + bondY + cpi + labor_force, data = econ_data)
summary(lm4)
###########################################################
lm1 <- lm(formula = changechanined~unemployed_percent + population, data = econ_data)
summary(lm1)

lm2 <- lm(formula = changecurr~unemployed_percent + cpi + bondY, data = econ_data)
summary(lm2)

lm3 <- lm(formula = changecurr~cpi, data = econ_data)
summary(lm3)

lm4 <- lm(formula = changecurr~unemployed_percent + population + bondY + cpi + labor_force, data = econ_data)
summary(lm4)


# create a graph of the adjusted R2 values of some of the models 
R2Data <- data.frame(modelName = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6" ),
                     modelR2Value= c(summary(lm1)$adj.r.squared, summary(lm2)$adj.r.squared, summary(lm3)$adj.r.squared, summary(lm4)$adj.r.squared, summary(lm5)$adj.r.squared, summary(lm6)$adj.r.squared))
R2Data %>% view()

ggplot(data=R2Data, aes(x=modelName, y=modelR2Value, fill=modelName)) +geom_bar(colour="black", stat="identity") + guides(fill=FALSE)


#######################################   Quarterly Economic Indicator Variables    ######################################################

# when getting data always run code to remove first column
qrt_data <- read_csv("qrtData.csv")
qrt_data <- qrt_data %>% select(!1) %>% view()
qrt_data %>% view()

#######################################   Quarterly Correlation Table    ######################################################

qrt_data_no_date <- qrt_data %>% select(!'DATE') %>% view()

cor(qrt_data_no_date) %>% view()

#######################################   Quarterly Economic Indicator Variables    ######################################################

rGDP_data <- read_csv("realGDP.csv") %>% rename(GDP = GDPC1) %>% view()
cpi_data <- read_csv("cpi.csv") %>% rename(cpi = CORESTICKM159SFRBATL) %>% view()
bondY_data <- read_csv("bondYields.csv") %>% rename(bond_yield = IRLTLT01USM156N) %>% view()
intRate_data <- read_csv("interestRate.csv") %>% rename(int_Rate = T10YFF) %>% view()
unEmployClaims_data <- read_csv("unemployClaims.csv") %>% rename(unemply_claims = IC4WSA) %>% view()
avgHoursWork_data <- read_csv("avgHoursWorked.csv") %>% rename(avg_hours_worked = PRS30006022) %>% view()
housingPermit_data <- read_csv("housingPermit.csv") %>% rename(housing_permits = PERMIT) %>% view()
# SP500_data <- read_csv("SP500.csv") %>% rename() %>% view()
consumerSent_data <- read_csv("consumerSent.csv") %>% rename(consumer_sent = UMCSENT_PCH) %>% view()
population_data <- read_csv("population.csv") %>% rename(population_perc_change = POPTHM_PCH) %>% view()
unemploymentRate_data <- read_csv("unemployRate.csv") %>% rename(unemployment_rate = UNRATE) %>% view()
perc_change_gdp <- read_csv("perc_gdp.csv") %>%  rename(perc_ch_GDP = GDPC1_PCH) %>% view()

qrtData <- rGDP_data %>% inner_join(cpi_data) %>% inner_join(bondY_data) %>% inner_join(intRate_data) %>% inner_join(unEmployClaims_data) %>% inner_join(avgHoursWork_data) %>% inner_join(housingPermit_data) %>% inner_join(population_data) %>% inner_join(unemploymentRate_data) %>% view()
qrtData %>% view()
write.csv(qrtData, "C:/Users/Grego/Desktop/usEconCSVs/qrtData.csv")

qrt_data <- qrt_data %>% inner_join(perc_change_gdp) %>% view()

write.csv(qrt_data, "C:/Users/Grego/Documents/490 Class/UsEconCSVs/qrt_data_new.csv")


lm12 <- lm(formula = GDP~cpi + bond_yield + int_Rate + unemply_claims + avg_hours_worked + housing_permits + population_perc_change + unemployment_rate, data = qrt_data)
summary(lm12)
plot(lm12)

lm2 <- lm(formula = GDP~cpi + bond_yield + int_Rate + unemply_claims, data = qrtData)
summary(lm2)

lm3a <- lm(formula = GDP~cpi, data = qrtData)
summary(lm3)
lm3b <- lm(formula = GDP~bond_yield, data = qrtData)
summary(lm3)
lm3c <- lm(formula = GDP~int_Rate, data = qrtData)
summary(lm3)
lm3d <- lm(formula = GDP~unemply_claims, data = qrtData)
summary(lm3)
lm3e <- lm(formula = GDP~avg_hours_worked, data = qrtData)
summary(lm3)
lm3f <- lm(formula = GDP~housing_permits, data = qrtData)
summary(lm3)
lm3g <- lm(formula = GDP~population_perc_change, data = qrtData)
summary(lm3)
lm3h <- lm(formula = GDP~unemployment_rate, data = qrtData)
summary(lm3)

qrt_data <- read_csv("qrt_data_new.csv") %>% view()

qrt_data_new <- qrt_data %>% select(!1) %>% view()

qrt_data_new %>% view()

qrt_data_new_no_date <- qrt_data_new %>% select(!'DATE') %>% view()

m <- cor(qrt_data_new_no_date)

corrplot(m, method='color')

cor(qrt_data_new_no_date) %>% view()
#######################################   Normalizing data w/ standard scaling    ######################################################

qrt_data <- read_csv("qrt_data_new.csv") %>% view()

qrt_data_new <- qrt_data_new %>% select(!1) %>% view()

qrt_data_new %>% view()

qrt_data_new_no_date <- qrt_data_new %>% select(!'DATE') %>% view()

qrt_data_norm <- scale(qrt_data_new_no_date) %>% view()

j <- cor(qrt_data_norm)

corrplot(j, method='color')

#######################################   Splitting data into training and testing    ######################################################

# reduce to 1:139 and take train sample from these then just take everything else for testing
qrt_train_size <- sample(1:139, size = 97, replace = FALSE) 

year_train_size <- sample(1:58, size = 41, replace = FALSE)

year_train_data <- econ_data %>% slice(year_train_size) %>% view()

year_test_data <- econ_data %>% slice(-year_train_size) %>% view()

qrt_train_data <- qrt_data %>% slice(qrt_train_size) %>% view()

qrt_test_data <- qrt_data %>% slice(-qrt_train_size) %>% view()

qrt_train_data_norm <- qrt_data_norm %>% slice(qrt_train_size) %>% view()

qrt_test_data_norm <- qrt_data_norm  %>% slice(-qrt_train_size) %>% view()

#######################################  Normalized data but same model as qrterly model 1   ######################################################
lm1_qrt_trainm <- lm(formula = perc_ch_GDP~cpi + bond_yield + int_Rate + unemply_claims + avg_hours_worked + housing_permits + population_perc_change + unemployment_rate, data = qrt_train_data_norm)

lm1_qrt_trainm <- lm(formula = GDP~cpi + bond_yield + int_Rate + unemply_claims + avg_hours_worked + housing_permits + population_perc_change + unemployment_rate, data = qrt_train_data_norm)
summary(lm1_qrt_trainm)
plot(lm1_qrt_trainm)

# compute the y-predictions using the linear model above on data that is has never seen before (Testing Data)
lm1_qrt_test_datam <- qrt_test_data_norm %>% mutate(ypred = predict(lm1_qrt_trainm, newdata = qrt_test_data_norm), residuals = perc_ch_GDP - ypred) %>% view()

# Cross validation correlation: When squared can be similarly to a R2 value

lm1_qrt_corr <- lm1_qrt_test_datam %>% summarize(cor = cor(perc_ch_GDP, ypred)) %>% mutate(R2 = cor^2, shrinkage = summary(lm1_qrt_trainm)$r.squared - R2) %>% view()

# Residuals plots for testing data
ggplot(lm1_qrt_test_datam, aes(x = ypred, y = residuals)) + geom_point() + geom_smooth(se = FALSE)
# we want the mean to be as close to zero (0) and the standard deviation to be close to the standard deviation from the training model 
lm1_qrt_test_datam %>% summarize(mean(residuals), sd(residuals))
# the training standard deviation is close to the testing standard deviation
sd(residuals(lm1_qrt_trainm))

#######################################   creating models from yearly data with training data    ######################################################


#######################################  Linear Model 1 Yearly w/ Training, Summary, Predictions, Residuals    ######################################################
lm1_year_train <- lm(formula = levelchained~unemployed_percent + population, data = year_train_data)
summary(lm1_year_train)

# Residual plots for training data
plot(lm1_year_train)

# compute the y-predictions using the linear model above on data that is has never seen before (Testing Data)
lm1_year_test_data <- year_test_data %>% mutate(ypred = predict(lm1_year_train, newdata = year_test_data), residuals = levelchained - ypred) %>% view()

# Cross validation correlation: When squared can be similarly to a R2 value

lm1_year_corr <- lm1_year_test_data %>% summarize(cor = cor(levelchained, ypred)) %>% mutate(R2 = cor^2, shrinkage = summary(lm1_year_train)$r.squared - R2) %>% view()

# Residuals plots for testing data
ggplot(lm1_year_test_data, aes(x = ypred, y = residuals)) + geom_point() + geom_smooth(se = FALSE)
# we want the mean to be as close to zero (0) and the standard deviation to be close to the standard deviation from the training model 
lm1_year_test_data %>% summarize(mean(residuals), sd(residuals))
# the training standard deviation is close to the testing standard deviation
sd(residuals(lm1_year_train))

#######################################  Linear Model 2 Yearly w/ Training, Summary, Predictions, Residuals    ######################################################

lm2_year_train <- lm(formula = levelchained~unemployed_percent + cpi + bondY, data = year_train_data)
summary(lm2_year_train)
# Residual plots for training data
plot(lm2_year_train)
#######################################  Linear Model 3 Yearly w/ Training, Summary, Predictions, Residuals    ######################################################

lm3_year_train <- lm(formula = levelchained~cpi, data = year_train_data)
summary(lm3_year_train)
# Residual plots for training data
plot(lm3_year_train)
#######################################  Linear Model 4 Yearly w/ Training, Summary, Predictions, Residuals    ######################################################

lm4_year_train <- lm(formula = levelchained~unemployed_percent + population + bondY + cpi + labor_force, data = year_train_data)
summary(lm4_year_train)
# Residual plots for training data
plot(lm4_year_train)
#######################################  Linear Model 5 Yearly w/ Training, Summary, Predictions, Residuals    ######################################################

lm5_year_train <- lm(formula = levelchained~unemployed_percent + population + bondY + cpi + labor_force + changecurr, data = year_train_data)
summary(lm5_year_train)
# Residual plots for training data
plot(lm5_year_train)
#######################################  Linear Model 6 Yearly w/ Training, Summary, Predictions, Residuals    ######################################################

lm6_year_train <- lm(formula = levelchained~unemployed_percent + bondY + changechained, data = year_train_data)
summary(lm6_year_train)
# Residual plots for training data
plot(lm6_year_train)

#######################################   Creating Models From Quarterly Data With Training Data    ######################################################

#######################################  Linear Model 1 Quarterly w/ Training, Summary, Predictions, Residuals    ######################################################

lm1_qrt_train <- lm(formula = perc_ch_GDP~cpi + bond_yield + int_Rate + unemply_claims + avg_hours_worked + housing_permits + population_perc_change + unemployment_rate, data = qrt_train_data)
summary(lm1_qrt_train)
# compute the y-predictions using the linear model above on data that is has never seen before (Testing Data)
lm1_qrt_test_data <- qrt_test_data %>% mutate(ypred = predict(lm1_qrt_train, newdata = qrt_test_data), residuals = perc_ch_GDP - ypred) %>% view()


lm1a_qrt_train <- lm(formula = GDP~cpi + bond_yield + int_Rate + unemply_claims + avg_hours_worked + housing_permits + population_perc_change + unemployment_rate, data = qrt_train_data)
summary(lm1a_qrt_train)
plot(lm1_qrt_train)


# compute the y-predictions using the linear model above on data that is has never seen before (Testing Data)
lm1a_qrt_test_data <- qrt_test_data %>% mutate(ypred = predict(lm1a_qrt_train, newdata = qrt_test_data), residuals = GDP - ypred) %>% view()
write.csv(lm1a_qrt_test_data, "C:/Users/Grego/Documents/490 Class/UsEconCSVs/1ypredGDP.csv")

# Cross validation correlation: When squared can be similarly to a R2 value

lm1_qrt_corr <- lm1_qrt_test_data %>% summarize(cor = cor(perc_ch_GDP, ypred)) %>% mutate(R2 = cor^2, shrinkage = summary(lm1_qrt_train)$r.squared - R2) %>% view()

write.csv(lm1_qrt_test_data, "C:/Users/Grego/Documents/490 Class/UsEconCSVs/1ypreda.csv")

# Residuals plots for testing data
ggplot(lm1_qrt_test_data, aes(x = ypred, y = residuals)) + geom_point() + geom_smooth(se = FALSE)
ggplot(lm1_qrt_test_data, aes(x = ypred, y = perc_ch_GDP)) + geom_point() 
# we want the mean to be as close to zero (0) and the standard deviation to be close to the standard deviation from the training model 
lm1_qrt_test_data %>% summarize(mean(residuals), sd(residuals))
# the training standard deviation is close to the testing standard deviation
sd(residuals(lm1_qrt_train))

metrics <- data.frame( R2 = R2(predict(lm1_qrt_train, newdata = qrt_test_data), qrt_test_data $ perc_ch_gdp),
                       RMSE = RMSE(predict(lm1_qrt_train, newdata = qrt_test_data), qrt_test_data $ perc_ch_gdp),
                       MAE = MAE(predict(lm1_qrt_train, newdata = qrt_test_data), qrt_test_data $ perc_ch_gdp))

metrics %>% view()

train_control <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)

model <- train(perc_ch_GDP ~cpi + bond_yield + int_Rate + unemply_claims + avg_hours_worked + housing_permits + population_perc_change + unemployment_rate, data = qrt_train_data, methods = 'lm', trControl = train_control)

print(model)

prediction <- predict(model, qrt_test_data)

print(prediction) 

train_control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)

model <- train(sales ~., data = marketing,
               method = "lm",
               trControl = train_control)

print(model)
#######################################  Linear Model 2 qrt w/ Training, Summary, Predictions, Residuals    ######################################################

lm2_qrt_train <- lm(formula = perc_ch_GDP~cpi + bond_yield + population_perc_change, data = qrt_train_data)
summary(lm2_qrt_train)

lm2_qrt_test_data <- qrt_test_data %>% mutate(ypred = predict(lm2_qrt_train, newdata = qrt_test_data), residuals = perc_ch_GDP - ypred) %>% view()
write.csv(lm2_qrt_test_data, "C:/Users/Grego/Documents/490 Class/UsEconCSVs/2ypred.csv")

# Cross validation correlation: When squared can be similarly to a R2 value

lm1_qrt_corr <- lm1_qrt_test_data %>% summarize(cor = cor(perc_ch_GDP, ypred)) %>% mutate(R2 = cor^2, shrinkage = summary(lm1_qrt_train)$r.squared - R2) %>% view()
#######################################  Linear Model 3 qrt w/ Training, Summary, Predictions, Residuals    ######################################################
lm3_qrt_train <- lm(formula = perc_ch_GDP~int_Rate + unemply_claims + avg_hours_worked + housing_permits + unemployment_rate, data = qrt_train_data)
summary(lm3_qrt_train)
lm3_qrt_test_data <- qrt_test_data %>% mutate(ypred = predict(lm3_qrt_train, newdata = qrt_test_data), residuals = perc_ch_GDP - ypred) %>% view()

write.csv(lm3_qrt_test_data, "C:/Users/Grego/Documents/490 Class/UsEconCSVs/3ypred.csv")

#######################################  Linear Model 3 qrt Graphs showing the adjusted R2 values of some models     ######################################################

R2Data <- data.frame(modelName = c("Model 1", "Model 2", "Model 3"),
                     modelR2Value= c(summary(lm1_qrt_train)$adj.r.squared, summary(lm2_qrt_train)$adj.r.squared, summary(lm3_qrt_train)$adj.r.squared))
R2Data %>% view()

ggplot(data=R2Data, aes(x=modelName, y=modelR2Value, fill=modelName)) +geom_bar(colour="black", stat="identity") + guides(fill=FALSE)

#######################################  Linear Model 4+ qrt w/ Training, Summary, Predictions, Residuals    ######################################################

lm3a_qrt_train <- lm(formula = GDP~cpi, data = qrt_train_data)
summary(lm3a_qrt_train)
lm3b_qrt_train <- lm(formula = GDP~bond_yield, data = qrt_train_data)
summary(lm3b_qrt_train)
lm3c_qrt_train <- lm(formula = GDP~int_Rate, data = qrt_train_data)
summary(lm3c_qrt_train)
lm3d_qrt_train <- lm(formula = GDP~unemply_claims, data = qrt_train_data)
summary(lm3d_qrt_train)
lm3e_qrt_train <- lm(formula = GDP~avg_hours_worked, data = qrt_train_data)
summary(lm3e_qrt_train)
lm3f_qrt_train <- lm(formula = GDP~housing_permits, data = qrt_train_data)
summary(lm3f_qrt_train)
lm3g_qrt_train <- lm(formula = GDP~population_perc_change, data = qrt_train_data)
summary(lmg3_qrt_train)
lm3h_qrt_train <- lm(formula = GDP~unemployment_rate, data = qrt_train_data)
summary(lm3h_qrt_train)
