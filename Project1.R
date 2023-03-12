library(tidyverse)
library(class)
library(ggplot2)

# P1: A DONE

setwd("C:/Users/Grego/Downloads") # Change the directory where the csv files can be found

covid19Data <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")

bedData <- read_csv("data.csv")

DemoData <- read_csv("demographics.csv")

covid19Data %>% view()
covid19Data %>% select(c(1:10, 300:340)) %>% view()
                     

bedData %>% view()

DemoData %>% view()

# P1: B DONE

b_covid19Data <- covid19Data %>% filter(is.na(Province_State), !is.na(Population)) %>% view()

# P1: C DONE

c_covid19Data <- b_covid19Data %>% pivot_longer(-c(1:12), names_to = 'Day', values_to = "shots", values_drop_na = TRUE) %>% view()

c_covid19Data <- c_covid19Data[,c(8,12:14)] %>% view()

c_covid19Data <- c_covid19Data %>% filter(shots > 0) %>% view() 

# P1: D DONE

d_covid19Data <- c_covid19Data %>% select(Country_Region, Population, shots) %>% group_by(Country_Region) %>% mutate(vacRate = shots/Population) %>% view()

# P1: E DONE

covid19Data_final <- d_covid19Data %>% group_by(Country_Region) %>% mutate(daysSinceStart = 1:n()) %>% view()

# P1: F: DONE

f_bedData <- bedData %>% group_by(Country) %>% summarize(year=max(Year), BedsPC=first(`Hospital beds (per 10 000 population)`)) %>% view()

bedData_final <- f_bedData[,c(1,3)] %>% view()

# P1: G: DONE

demo_tidy <- DemoData %>% select(-`Series Name`) %>% pivot_wider(names_from = "Series Code", values_from = YR2015)
demo_merged <- demo_tidy %>% mutate(SP.POP.80UP=SP.POP.80UP.FE+SP.POP.80UP.MA) %>% mutate(SP.POP.1564.IN=SP.POP.1564.MA.IN+SP.POP.1564.FE.IN) %>% mutate(SP.POP.0014.IN=SP.POP.0014.MA.IN+SP.POP.0014.FE.IN) %>% mutate(SP.DYN.AMRT=SP.DYN.AMRT.MA+SP.DYN.AMRT.FE) %>% mutate(SP.POP.TOTL.IN=SP.POP.TOTL.FE.IN+SP.POP.TOTL.MA.IN) %>% mutate(SP.POP.65UP.IN=SP.POP.65UP.FE.IN+SP.POP.65UP.MA.IN) %>% select(-contains(".FE")) %>% select(-contains(".MA"))
demo_merged %>% view()
demo_rows <- demo_merged[,c(1,3,4)] %>% view()
demo_final <- demo_rows %>% filter(!is.na(SP.DYN.LE00.IN), !is.na(SP.URB.TOTL)) %>% view()

demo_final %>% view()
# P1: H DONE
# P2: Change country names so they are the same, before joining tables
# Note: South Koreas data is not in the covid19 data set so South Korea is not 
# in the final data, even though I mutated the data for it.
demo_final <- demo_final %>% mutate(`Country Name` = replace(`Country Name`, `Country Name` == "Korea, Rep.", "South Korea")) 
demo_final <- demo_final %>% mutate(`Country Name` = replace(`Country Name`, `Country Name` == "Iran, Islamic Rep.", "Iran"))

bedData_final <- bedData_final %>% mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea")) 
bedData_final <- bedData_final %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) 
bedData_final <- bedData_final %>% mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran")) 

covid19Data_final %>% view()

bedData_final %>% view()

demo_final %>% view()


join1 <- covid19Data_final %>% inner_join(bedData_final, by=c(Country_Region="Country")) %>% view()
finalData <- join1 %>% inner_join(demo_final, by=c(Country_Region="Country Name"))
finalData %>% view()

# P3 A: Variable Combination
combinedFinal_data <- finalData %>% mutate(UrbanPopPercent = (Population/SP.URB.TOTL)) %>% view()

# P3 B: Modeling

lm1 <- lm(formula = vacRate~UrbanPopPercent + BedsPC, data = combinedFinal_data)
summary(lm1)

lm2 <- lm(formula = vacRate~UrbanPopPercent + Population + SP.DYN.LE00.IN + daysSinceStart + SP.URB.TOTL + BedsPC, data = combinedFinal_data)
summary(lm2)

lm3 <- lm(formula = vacRate~UrbanPopPercent + BedsPC + daysSinceStart, data = combinedFinal_data)
summary(lm3)

lm4 <- lm(formula = vacRate~Population + BedsPC + SP.DYN.LE00.IN, data = combinedFinal_data)
summary(lm4)

lm5 <- lm(formula = vacRate~Population + SP.URB.TOTL + BedsPC, data = combinedFinal_data)
summary(lm5)

lm6 <- lm(formula = vacRate~UrbanPopPercent + daysSinceStart + BedsPC + SP.DYN.LE00.IN, data = combinedFinal_data)
summary(lm6)
# summary(lm6)$adj.r.squared

# P4: A CHECK PDF
# P4: B CHECK PDF
# P4: C CHECK PDF

# P4: D DONE

# get the highest vaccination rate for each country, days since start of vaccinating, and the country
recent_vacRate <- finalData %>% group_by(Country_Region) %>% top_n(daysSinceStart, n=1) %>% select(vacRate, daysSinceStart, Country_Region) %>% view()

# graph the highest vaccinating rate with days since start of vaccination

ggplot(recent_vacRate, aes(daysSinceStart, vacRate)) + geom_point() # WORKING

# P4: E SEE PDF FOR GRAPH

R2Data <- data.frame(modelName  = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6" ),
                       modelR2Value= c(summary(lm1)$adj.r.squared, summary(lm2)$adj.r.squared, summary(lm3)$adj.r.squared, summary(lm4)$adj.r.squared, summary(lm5)$adj.r.squared, summary(lm6)$adj.r.squared))
R2Data %>% view()

ggplot(data=R2Data, aes(x=modelName, y=modelR2Value, fill=modelName)) +geom_bar(colour="black", stat="identity") + guides(fill=FALSE)
# P4: F CHECK PDF






