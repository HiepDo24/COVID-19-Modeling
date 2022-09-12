#DATA PREPARATION/WRANGLING to get all the data into one table that can be used 
#for linear modeling

# a) reading the data files using read_csv()

library(tidyverse)
library(modelr)
library(ggplot2)
#
# setwd("~/R/cpsc_375_project")

vaccine_data_global <- read_csv(
    paste("https://raw.githubusercontent.com/",
          "govex/COVID-19/master/data_tables/",
          "vaccine_data/global_data/",
          "time_series_covid19_vaccine_doses_admin_global.csv", sep=""))

gdp_data_global <- read_csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3011433.csv")

demographics_data_global <- read_csv("demographics.csv")

######################################################################
# b) Removing unneeded rows and columns. 

vaccine_data_global_reduced <- vaccine_data_global %>% 
    select(iso3, Country_Region, Population, 14:ncol(.))

vaccine_data_global_reduced <- na.omit(vaccine_data_global_reduced)

gdp_data_global_reduced <- gdp_data_global %>% 
    rename(iso3 = `Country Code`, Country_Region = `Country Name`) %>% 
    select(!starts_with("Indicator"))

demographics_data_global_reduced <- demographics_data_global %>% 
    rename(iso3 = `Country Code`, Country_Region = `Country Name`) %>% 
    select(!`Series Name`)

######################################################################
# c) tidying tables, as needed. For example, the vaccination data is not tidy.
# picking the column names with year starting with 2 and putting them into a 
# new "Date" column. Taking the values from the specific day columns and
# putting the values into a new column named "shots". Dropping any NA values.

vaccine_data_global_reduced_tidy <- vaccine_data_global_reduced %>% 
    pivot_longer(cols = starts_with("2"), names_to = "Date",
                 values_to = "shots", values_drop_na = TRUE)

# picking column names of 3 to the end and making them the values in a new
# column called "Year". Adding the cell values from these columns and making
# a new column called "GDP". Dropping an NA values.
gdp_data_global_reduced_tidy <- gdp_data_global_reduced %>% 
    pivot_longer(cols = 3:ncol(.), names_to = "Year",
                 values_to = "GDP", values_drop_na = TRUE)

# arranging gdp_data_global_reduced_tidy by Country_Region and Year
# and grouping Country_Region values together and summarize_all applies to all
# variables in Year and the last Year is returned.
gdp_data_global_reduced_tidy <- gdp_data_global_reduced_tidy %>% 
    arrange(Country_Region, Year) %>%  
    group_by(Country_Region) %>% summarize_all(last)

# Making each Country as an observation in a single row by making unique 
# "Series Code", which represent the demographic value into subsequent column 
# names, and the population value corresponding to that demographic as a value
# with column name "YR2015"
demographics_data_global_reduced_tidy <- demographics_data_global_reduced %>%
    pivot_wider(names_from = `Series Code`, values_from = YR2015)

######################################################################
# d) Calculate the vaccination rate: vaccinations/population
vaccine_data_global_reduced_tidy %>% mutate(vacRate = shots/Population)

######################################################################
# e) Since the most important factor affecting vaccination rate is the number of 
# days since vaccination began (vaccination rate always increases), calculate a 
# variable that is: number of days since first non-zero vaccination number. 
# This variable will be important for modeling. 

vaccine_data_global_reduced_tidy_mutate <- vaccine_data_global_reduced_tidy %>% 
    mutate(vacRate = shots/Population) %>% filter(shots > 0, na.rm = TRUE) %>%
    group_by(Country_Region) %>% 
    mutate(daysSinceStart = row_number()) 

 path <- "~/R/cpsc_375_project"
 vaccine_data_global_reduced_tidy_mutate %>% write.csv(file.path(path, "trouble.csv"), row.names = FALSE)

  ggplot(data = vaccine_data_global_reduced_tidy_mutate) + geom_histogram(mapping = aes(daysSinceStart))

######################################################################
# f) Discard data that is not needed. For example, only the GDP of the most 
#    recent year is necessary.

vaccine_data_global_reduced_tidy_mutate_clean <- 
    vaccine_data_global_reduced_tidy_mutate %>% 
    select(iso3, Country_Region, vacRate, shots, Population, daysSinceStart)

gdp_data_global_reduced_tidy_clean <- gdp_data_global_reduced_tidy %>% 
    select(-Country_Region,-Year) 

demographics_data_global_reduced_tidy_clean <- 
    demographics_data_global_reduced_tidy %>% 
    select(-Country_Region) 

######################################################################
# g) You can ignore sex-related differences in demographics in this project, 
#   so add the male/female population numbers together.

demographics_data_global_reduced_tidy_clean_unisex <- 
  demographics_data_global_reduced_tidy_clean %>% 
      mutate(SP.POP.80UP = SP.POP.80UP.FE + SP.POP.80UP.MA) %>%
      mutate(SP.POP.1564.IN = SP.POP.1564.MA.IN + SP.POP.1564.FE.IN) %>%
      mutate(SP.POP.0014.IN = SP.POP.0014.MA.IN + SP.POP.0014.FE.IN) %>%
      mutate(SP.DYN.AMRT = (SP.DYN.AMRT.FE + SP.DYN.AMRT.MA) / 2 ) %>%
      mutate(SP.POP.65UP.IN = SP.POP.65UP.FE.IN + SP.POP.65UP.MA.IN) %>% 
      select(c("iso3", "SP.DYN.LE00.IN", "SP.URB.TOTL", "SP.POP.TOTL", 
           "SP.POP.80UP":ncol(.)))

######################################################################
# h) Merge all tables (Hint: Join using the 3-letter ISO code for a country)

vaccine_data_global_table <- vaccine_data_global_reduced_tidy_mutate_clean

gdp_data_global_table <- gdp_data_global_reduced_tidy_clean

demographics_data_global_table <- demographics_data_global_reduced_tidy_clean_unisex

merged_table <- vaccine_data_global_table %>% inner_join(gdp_data_global_table) %>% inner_join(demographics_data_global_table) %>% view()

path <- "~/R/cpsc_375_project"
merged_table %>% write.csv(file.path(path, "merged_table.csv"), row.names = FALSE)

######################################################################
######################################################################
######################################################################
# 2 LINEAR MODELING THE COVID VACCINATION RATE

merged_table <- na.omit(merged_table)

# Make a list of all predictor variables that are available.
predicor_df <- merged_table %>% ungroup() %>% select(5:15) 
predicor_df_names <- colnames(predicor_df)

# PREDICTOR VARRIABLES
# "Population"     
# "daysSinceStart" 
# "GDP"            
# "SP.DYN.LE00.IN" is Life expectancy at birth, total (years)
# "SP.URB.TOTL"    
# "SP.POP.TOTL"    
# "SP.POP.80UP"    
# "SP.POP.1564.IN"
# "SP.POP.0014.IN" 
# "SP.DYN.AMRT" is mortality rate
# "SP.POP.65UP.IN"

# > cor(x = merged_table[,5:15], y = merged_table$vacRate)
# [,1]
# Population     -0.040637932
# daysSinceStart  0.854192242
# GDP             0.033136635
# SP.DYN.LE00.IN  0.272942147
# SP.URB.TOTL    -0.016357711
# SP.POP.TOTL    -0.038453415
# SP.POP.80UP     0.009860849
# SP.POP.1564.IN -0.033146515
# SP.POP.0014.IN -0.064286339
# SP.DYN.AMRT    -0.235641135
# SP.POP.65UP.IN -0.003737585


#TRANSFORMED 



predicor_df %>% mutate()

cor(x = merged_table[,5:15], y = merged_table$vacRate)


sum(!is.na(merged_table$SP.POP.0014.IN))

m6_df <- merged_table %>% filter(!is.na(SP.POP.0014.IN)) %>% view()
cor(x = m6_df[,5:15], y = m6_df$vacRate)

m1 <- lm(formula = vacRate~daysSinceStart, data = merged_table)
m2 <- lm(formula = vacRate~daysSinceStart+Population, data = merged_table)
m3 <- lm(formula = vacRate~Population+GDP, data = merged_table)
m4 <- lm(formula = vacRate~GDP+SP.URB.TOTL, data = merged_table)
m5 <- lm(formula = vacRate~Population+SP.POP.TOTL, data = merged_table)

#geom_abline(slope= 0.003976, intercept = -0.026597)

summary <- c(summary(m1)$r.squared, summary(m2)$r.squared,summary(m3)$r.squared,summary(m4)$r.squared,summary(m5)$r.squared)
barplot(summary)

m6 <- lm(formula = vacRate~GDP+SP.POP.0014.IN, data = m6_df)
summary(m6)
summary(m1)

# m7 - Multiple R-squared:  0.7952
m7 <- lm(formula = vacRate~daysSinceStart+SP.DYN.LE00.IN, data = merged_table)
summary(m7)

merged_table_max_daysSinceStart <- merged_table %>% group_by(Country_Region) %>% 
  summarize_all(last) %>% view()

ggplot(merged_table_max_daysSinceStart) + 
  geom_point(mapping = aes(x = daysSinceStart, y = vacRate )) + 
  geom_abline(slope = 0.00397, intercept =  -0.02554 )

merged_table_population <- merged_table %>% group_by(Country_Region) %>% slice(n())

ggplot(merged_table_population) + 
  geom_point(mapping = aes(x = Population, y = vacRate )) + 
  geom_abline(slope = -1.037e-10  , intercept =  1.066e+00 )

merged_table_GDP <- merged_table %>% group_by(Country_Region) %>% slice(n())

ggplot(merged_table_GDP) + 
  geom_point(mapping = aes(x = GDP, y = vacRate )) + 
  geom_abline(slope = 7.600e-15   , intercept =  1.049e+00 )

merged_table_SP.URB.TOTL <- merged_table %>% group_by(Country_Region) %>% slice(n())

ggplot(merged_table_SP.URB.TOTL) + 
  geom_point(mapping = aes(x = SP.URB.TOTL, y = vacRate )) + 
  geom_abline(slope = -1.006e-10   , intercept =  1.062e+00 )

merged_table_SP.POP.TOTL <- merged_table %>% group_by(Country_Region) %>% slice(n())

ggplot(merged_table_SP.POP.TOTL) + 
  geom_point(mapping = aes(x = SP.POP.TOTL, y = vacRate )) + 
  geom_abline(slope = -1.023e-10   , intercept =  1.065e+00)





