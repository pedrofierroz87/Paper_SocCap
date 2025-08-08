rm(list=ls())

options(scipen=999, # avoid scientific notation
        stringsAsFactors = FALSE, digits = 2)

options(digits = 7)

library(ggplot2)
library(tidyr)
library(dplyr)
library(sf)
library(spdep)
library(foreign)
library(haven)
library(car)
library(broom)
library(interactions)
library(tigris)
library(corrplot)
library(stringr)
library(effects)

data_trump <- read_dta("Data_SocCap/Golfing_Trump.dta")
data_meta <- read.csv("Data_SocCap/social_capital_county.csv")

# Creating variable for employment change 80-2016. Assuming EMP_AVE_80 and EMP_AVE_16 are the relevant variables
data_trump$Employment_Change = data_trump$EMP_AVE_16 - data_trump$EMP_AVE_80

##### Merging Data #####

data_trump$FIPS_ID <- as.numeric(data_trump$FIPS_ID)
data_meta$county <- as.numeric(data_meta$county)

# Find counties in data_trump not in data_meta
unmatched_in_trump <- setdiff(data_trump$FIPS_ID, data_meta$county)
unmatched_in_trump

# Find counties in data_meta not in data_trump
unmatched_in_meta <- setdiff(data_meta$county, data_trump$FIPS_ID)
unmatched_in_meta

data_trump2 <- merge(data_trump, data_meta, by.x = "FIPS_ID", by.y = "county", all = FALSE)
names(data_trump2)


counties <- st_read("Shapefile_SocCap/counties.shp")
counties <- counties %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) # Excluding Alaska, Hawaii, and territories

counties$GEOID <- as.numeric(counties$GEOID)
summary(counties$GEOID)

counties <- merge(data_trump2, counties, by.x = "FIPS_ID", by.y = "GEOID", all.x = FALSE, all.y = TRUE)


##### ANALYSES #####

##### 1. Replicating Basic Model #####

# For 2016
model16_1 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + factor(S_ID), data = data_trump)
summary(model16_1)
model16_3 <- lm(MAR_T_new ~ INC_PC_16 + SC5_PCM_14 + factor(S_ID), data = data_trump)
summary(model16_3)
model16_5 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + SC5_PCM_14 + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump)
summary(model16_5)

# Tidy the model and filter to include only SC5_PCM_14 and the intercept
tidy_model_filtered <- tidy(model16_5) %>%
  filter(term == "SC5_PCM_14")

tidy_model_filtered$term <- ifelse(tidy_model_filtered$term == "SC5_PCM_14", "Comprehensive Social Capital", tidy_model_filtered$term)

# Create the coefficient plot with updated term labels
ggplot(tidy_model_filtered, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add horizontal line which becomes vertical after coord_flip
  coord_flip() +  # Make plot horizontal
  labs(title = "Coefficient Plot for 2016 Elections", x = "Term", y = "Estimate") +
  theme_minimal()



# For 2020
model20_1 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + factor(S_ID), data = data_trump)
summary(model20_1)
model20_3 <- lm(MAR_T_20_new ~ INC_PC_16 + SC5_PCM_14 + factor(S_ID), data = data_trump)
summary(model20_3)
model20_5 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + SC5_PCM_14 + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump)
summary(model20_5)

# Tidy the model and filter to include only SC5_PCM_14 and the intercept
tidy_model20_filtered <- tidy(model20_5) %>%
  filter(term == "SC5_PCM_14")

tidy_model20_filtered$term <- ifelse(tidy_model20_filtered$term == "SC5_PCM_14", "Comprehensive Social Capital", tidy_model20_filtered$term)

# Create the coefficient plot with updated term labels
ggplot(tidy_model20_filtered, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add horizontal line which becomes vertical after coord_flip
  coord_flip() +  # Make plot horizontal
  labs(title = "Coefficient Plot for 2020 Elections", x = "Term", y = "Estimate") +
  theme_minimal()


##### 2. Testing Basic Model With Meta Data #####

# For 2016

# Civic Engagement
modelnew16_1 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew16_1)
modelnew16_2 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew16_2)

# Social Cohesion
modelnew16_3 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew16_3)
modelnew16_4 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew16_4)

# Economic Connectedness
modelnew16_5 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew16_5)


# For 2020

# Civic Engagement
modelnew20_1 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew20_1)
modelnew20_2 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew20_2)

# Social Cohesion
modelnew20_3 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew20_3)
modelnew20_4 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew20_4)

# Economic Connectedness
modelnew20_5 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump2)
summary(modelnew20_5)


##### 3. Scatterplots for social capital and Trump Margin #####

ggplot(data_trump2, aes(x = SC5_PCM_14, y = MAR_T_new, size = POP_DEN_16)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add linear regression line
  labs(title = "Scatter Plot with Linear Trend",
       x = "Social Capital",
       y = "Trump Margin") +  # Removed size label here as the legend is being removed
  theme_minimal() +
  scale_size_continuous(range = c(1, 10)) +
  guides(size = FALSE)  # Remove the size legend


ggplot(data_trump2, aes(x = civic_organizations_county, y = MAR_T_new, size = POP_DEN_16)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add linear regression line
  labs(title = "Scatter Plot with Linear Trend",
       x = "Civic Organizations",
       y = "Trump Margin") +  # Removed size label here as the legend is being removed
  theme_minimal() +
  scale_size_continuous(range = c(1, 10)) +
  guides(size = FALSE)  # Remove the size legend

ggplot(data_trump2, aes(x = volunteering_rate_county, y = MAR_T_new, size = POP_DEN_16)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add linear regression line
  labs(title = "Scatter Plot with Linear Trend",
       x = "Volunteering Rates",
       y = "Trump Margin") +  # Removed size label here as the legend is being removed
  theme_minimal() +
  scale_size_continuous(range = c(1, 10)) +
  guides(size = FALSE)  # Remove the size legend

ggplot(data_trump2, aes(x = clustering_county, y = MAR_T_new, size = POP_DEN_16)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add linear regression line
  labs(title = "Scatter Plot with Linear Trend",
       x = "Friendship Clastering",
       y = "Trump Margin") +  # Removed size label here as the legend is being removed
  theme_minimal() +
  scale_size_continuous(range = c(1, 10)) +
  guides(size = FALSE)  # Remove the size legend


ggplot(data_trump2, aes(x = support_ratio_county, y = MAR_T_new, size = POP_DEN_16)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add linear regression line
  labs(title = "Scatter Plot with Linear Trend",
       x = "Support Networks",
       y = "Trump Margin") +  # Removed size label here as the legend is being removed
  theme_minimal() +
  scale_size_continuous(range = c(1, 10)) +
  guides(size = FALSE)  # Remove the size legend

ggplot(data_trump2, aes(x = ec_county, y = MAR_T_new, size = POP_DEN_16)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add linear regression line
  labs(title = "Scatter Plot with Linear Trend",
       x = "Economic Connectedness",
       y = "Trump Margin") +  # Removed size label here as the legend is being removed
  theme_minimal() +
  scale_size_continuous(range = c(1, 10)) +
  guides(size = FALSE)  # Remove the size legend

##### 4. Histograms ##### 

# Separate Plots
ggplot(data_trump, aes(x = MAR_T_new)) + 
  geom_histogram(fill = "red", color = "black") + 
  labs(title = "Trump Margin for 2016", x = "Margin", y = "Frequency") +
  theme_minimal()

ggplot(data_trump, aes(x = MAR_T_20_new)) + 
  geom_histogram(fill = "red", color = "black") + 
  labs(title = "Trump Margin for 2020", x = "Margin", y = "Frequency") +
  theme_minimal()

# Unique plot
p <- ggplot() +
  # Histogram for MAR_T_new (2016)
  geom_histogram(data = data_trump, aes(x = MAR_T_new, y = ..density..), 
                 fill = "blue", alpha = 0.5, bins = 70, color = "black") +
  # Histogram for MAR_T_20_new (2020)
  geom_histogram(data = data_trump, aes(x = MAR_T_20_new, y = ..density..), 
                 fill = "green", alpha = 0.5, bins = 70, color = "black") +
  labs(title = "Comparison of Trump Margin: 2016 vs. 2020", 
       x = "Margin", y = "Frequency") +
  theme_minimal() +
  # Adding a legend manually
  scale_fill_manual("", labels = c("2016", "2020"), values = c("blue", "green"))

# Display the plot
print(p)


##### 5. Maping #####

# Maping Margin Votes

ggplot(data = counties) +
  geom_sf(aes(fill = MAR_T_new, geometry = geometry), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "US Counties", option = "C") +
  labs(title = "Trump Margin 2016") +
  theme_minimal()

ggplot(data = counties) +
  geom_sf(aes(fill = MAR_T_20_new, geometry = geometry), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "US Counties", option = "C") +
  labs(title = "Trump Margin 2020") +
  theme_minimal()

# Maping Social Capital

ggplot(data = counties) +
  geom_sf(aes(fill = ec_county, geometry = geometry), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "US Counties", option = "C") +
  labs(title = "Economic Connectedness") +
  theme_minimal()

ggplot(data = counties) +
  geom_sf(aes(fill = volunteering_rate_county, geometry = geometry), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "US Counties", option = "C") +
  labs(title = "Volunteering Rate") +
  theme_minimal()

ggplot(data = counties) +
  geom_sf(aes(fill = civic_organizations_county, geometry = geometry), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "US Counties", option = "C") +
  labs(title = "Civic Organizations") +
  theme_minimal()

ggplot(data = counties) +
  geom_sf(aes(fill = clustering_county, geometry = geometry), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "US Counties", option = "C") +
  labs(title = "Friendship Clustering") +
  theme_minimal()

ggplot(data = counties) +
  geom_sf(aes(fill = support_ratio_county, geometry = geometry), color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "US Counties", option = "C") +
  labs(title = "Support Networks") +
  theme_minimal()



##### 7. Interactions 2016 ##### 

# For 2016 Employment Change

# Civic Engagement
modelint16_1 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*civic_organizations_county + factor(S_ID), data = data_trump2)
summary(modelint16_1)

modelint16_2 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*volunteering_rate_county + factor(S_ID), data = data_trump2)
summary(modelint16_2)

# Social Cohesion
modelint16_3 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*clustering_county + factor(S_ID), data = data_trump2)
summary(modelint16_3)
modelint16_4 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*support_ratio_county + factor(S_ID), data = data_trump2)
summary(modelint16_4)

# Economic Connectedness
modelint16_5 <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*ec_county + factor(S_ID), data = data_trump2)
summary(modelint16_5)

# For 2016 Population Change

# Civic Engagement
modelint16_1pop <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*civic_organizations_county + factor(S_ID), data = data_trump2)
summary(modelint16_1pop)

modelint16_2pop <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*volunteering_rate_county + factor(S_ID), data = data_trump2)
summary(modelint16_2pop)

# Social Cohesion
modelint16_3pop <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*clustering_county + factor(S_ID), data = data_trump2)
summary(modelint16_3pop)
modelint16_4pop <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*support_ratio_county + factor(S_ID), data = data_trump2)
summary(modelint16_4pop)

# Economic Connectedness
modelint16_5pop <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*ec_county + factor(S_ID), data = data_trump2)
summary(modelint16_5pop)

# For 2016 Average Earnings per Job Change

# Civic Engagement
modelint16_1ear <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*civic_organizations_county + factor(S_ID), data = data_trump2)
summary(modelint16_1ear)

modelint16_2ear <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*volunteering_rate_county + factor(S_ID), data = data_trump2)
summary(modelint16_2ear)

# Social Cohesion
modelint16_3ear <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*clustering_county + factor(S_ID), data = data_trump2)
summary(modelint16_3ear)
modelint16_4ear <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*support_ratio_county + factor(S_ID), data = data_trump2)
summary(modelint16_4ear)

# Economic Connectedness
modelint16_5ear <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*ec_county + factor(S_ID), data = data_trump2)
summary(modelint16_5ear)

# For 2016 wages and salaries Change

# Civic Engagement
modelint16_1sal <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*civic_organizations_county + factor(S_ID), data = data_trump2)
summary(modelint16_1sal)

modelint16_2sal <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*volunteering_rate_county + factor(S_ID), data = data_trump2)
summary(modelint16_2sal)

# Social Cohesion
modelint16_3sal <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*clustering_county + factor(S_ID), data = data_trump2)
summary(modelint16_3sal)
modelint16_4sal <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*support_ratio_county + factor(S_ID), data = data_trump2)
summary(modelint16_4sal)

# Economic Connectedness
modelint16_5sal <- lm(MAR_T_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*ec_county + factor(S_ID), data = data_trump2)
summary(modelint16_5sal)





##### 8. Interactions 2020 ##### 

# For 2020 Employment Change

# Civic Engagement
modelint20_1 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*civic_organizations_county + factor(S_ID), data = data_trump2)
summary(modelint20_1)

modelint20_2 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*volunteering_rate_county + factor(S_ID), data = data_trump2)
summary(modelint20_2)

# Social Cohesion
modelint20_3 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*clustering_county + factor(S_ID), data = data_trump2)
summary(modelint20_3)
modelint20_4 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*support_ratio_county + factor(S_ID), data = data_trump2)
summary(modelint20_4)

# Economic Connectedness
modelint20_5 <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EMP_AVE_CGR80_19*ec_county + factor(S_ID), data = data_trump2)
summary(modelint20_5)

# For 2020 Population Change

# Civic Engagement
modelint20_1pop <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*civic_organizations_county + factor(S_ID), data = data_trump2)
summary(modelint20_1pop)

modelint20_2pop <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*volunteering_rate_county + factor(S_ID), data = data_trump2)
summary(modelint20_2pop)

# Social Cohesion
modelint20_3pop <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*clustering_county + factor(S_ID), data = data_trump2)
summary(modelint20_3pop)
modelint20_4pop <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*support_ratio_county + factor(S_ID), data = data_trump2)
summary(modelint20_4pop)

# Economic Connectedness
modelint20_5pop <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + POP_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + POP_CGR80_19*ec_county + factor(S_ID), data = data_trump2)
summary(modelint20_5pop)

# For 2020 Average Earnings per Job Change

# Civic Engagement
modelint20_1ear <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*civic_organizations_county + factor(S_ID), data = data_trump2)
summary(modelint20_1ear)

modelint20_2ear <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*volunteering_rate_county + factor(S_ID), data = data_trump2)
summary(modelint20_2ear)

# Social Cohesion
modelint20_3ear <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*clustering_county + factor(S_ID), data = data_trump2)
summary(modelint20_3ear)
modelint20_4ear <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*support_ratio_county + factor(S_ID), data = data_trump2)
summary(modelint20_4ear)

# Economic Connectedness
modelint20_5ear <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + EAR_AVE_JOB_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + EAR_AVE_JOB_CGR80_19*ec_county + factor(S_ID), data = data_trump2)
summary(modelint20_5ear)

# For 2020 wages and salaries Change

# Civic Engagement
modelint20_1sal <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + civic_organizations_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*civic_organizations_county + factor(S_ID), data = data_trump2)
summary(modelint20_1sal)

modelint20_2sal <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + volunteering_rate_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*volunteering_rate_county + factor(S_ID), data = data_trump2)
summary(modelint20_2sal)

# Social Cohesion
modelint20_3sal <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + clustering_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*clustering_county + factor(S_ID), data = data_trump2)
summary(modelint20_3sal)
modelint20_4sal <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + support_ratio_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*support_ratio_county + factor(S_ID), data = data_trump2)
summary(modelint20_4sal)

# Economic Connectedness
modelint20_5sal <- lm(MAR_T_20_new ~ INC_PC_16 + GINI_16 + WA_SA_MEA_CGR80_19 + ec_county + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + WA_SA_MEA_CGR80_19*ec_county + factor(S_ID), data = data_trump2)
summary(modelint20_5sal)






############## CODE ROWE ##############
plot_models(modelnew16_1, modelnew16_2,
            rm.terms = "factor(S_ID) [AL,AZ,AR,CA,CO,CT,DE,DC,FL,GA,HI,ID,IL,IN,IA,KS,KY,LA,ME,MD,MA,MI,MN,MS,MO,MT,NE,NV,NH,NJ,NM,NY,NC,ND,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VT,VA,WA,WV,WI,WY]", 
            axis.labels = c(
              "Income per capita (2016)", "Inequality (Gini 2016)", "Employment change (1980-2016)", "Social Capital Community", "Density (2016)", "Unemployment rate (2016)", "Education (2016)", "Share of black population (2016)", "Sex ratio, males (2016)", "Age dependency, young (2016)", "Share Married (2016)")
)






############## TESTING DATA R AND R ##############
presdata <- read.csv("Data_SocCap/Presidential_Results_1216.tab")


# Calculate Compound Growth Rate (log difference divided by years)
data_trump$POP_CGR80_19_calc <- (log(data_trump$POP_19) - log(data_trump$POP_80)) / (2019 - 1980)

# Check correlation between calculated and original CGR
cor(data_trump$POP_CGR80_19_calc, data_trump$POP_CGR80_19, use = "complete.obs")
# Check if values are almost identical (should be very close to 0)
max(abs(data_trump$POP_CGR80_19_calc - data_trump$POP_CGR80_19), na.rm = TRUE)
data_trump[1:10, c("POP_CGR80_19_calc", "POP_CGR80_19")]



#####################
# If you have 2019 value already logged and 1980 raw:
data_trump$EAR_AVE_JOB_CGR80_19_calc <- (
  data_trump$EAR_AVE_JOB_19_ln - data_trump$EAR_AVE_JOB_80_ln
) / (2019 - 1980)

# Compare with original variable
summary(data_trump$EAR_AVE_JOB_CGR80_19_calc)
summary(data_trump$EAR_AVE_JOB_CGR80_19)

# Correlation check
cor(data_trump$EAR_AVE_JOB_CGR80_19_calc, data_trump$EAR_AVE_JOB_CGR80_19, use = "complete.obs")
# Max absolute difference
max(abs(data_trump$EAR_AVE_JOB_CGR80_19_calc - data_trump$EAR_AVE_JOB_CGR80_19), na.rm = TRUE)

data_trump[1:10, c("EAR_AVE_JOB_CGR80_19_calc", "EAR_AVE_JOB_CGR80_19")]







###################
# Compute CGR for log-transformed WA_SA_MEA
data_trump$WA_SA_MEA_ln_D80_19_calc <- (
  data_trump$WA_SA_MEA_19_ln - data_trump$WA_SA_MEA_80_ln
) / (2019 - 1980)

# Compare with the original variable
summary(data_trump$WA_SA_MEA_ln_D80_19_calc)
cor(data_trump$WA_SA_MEA_ln_D80_19_calc, data_trump$WA_SA_MEA_CGR80_19, use = "complete.obs")
max(abs(data_trump$WA_SA_MEA_ln_D80_19_calc - data_trump$WA_SA_MEA_CGR80_19), na.rm = TRUE)
data_trump[1:10, c("WA_SA_MEA_ln_D80_19_calc", "WA_SA_MEA_CGR80_19")]



#####################
# Recreate the compound growth rate for EMP_AVE
data_trump$EMP_AVE_CGR80_19_calc <- (
  log(data_trump$EMP_AVE_19) - log(data_trump$EMP_AVE_80)) / (2019 - 1980)
# Correlation check
cor(data_trump$EMP_AVE_CGR80_19_calc, data_trump$EMP_AVE_CGR80_19, use = "complete.obs")
max(abs(data_trump$EMP_AVE_CGR80_19_calc - data_trump$EMP_AVE_CGR80_19), na.rm = TRUE)

data_trump[1:10, c("EMP_AVE_CGR80_19_calc", "EMP_AVE_CGR80_19")]



# Calculate Compound Growth Rate for EMP_AVE
data_trump$EMP_AVE_CGR80_19_calc <- ((data_trump$EMP_AVE_19 / data_trump$EMP_AVE_80)^(1/(2019-1980))) - 1

# Compare with original variable
summary(data_trump$EMP_AVE_CGR80_19_calc)
cor(data_trump$EMP_AVE_CGR80_19_calc, data_trump$EMP_AVE_CGR80_19, use = "complete.obs")

# Print first 10 rows for inspection
head(data_trump[, c("EMP_AVE_80", "EMP_AVE_19", "EMP_AVE_CGR80_19", "EMP_AVE_CGR80_19_calc")], 10)




summary(data_trump$EDU1_16)
summary(data_trump$EDU2_16)
summary(data_trump$EDU3_16)
summary(data_trump$EDU4_16)

data_trump$EDU_total_16 <- data_trump$EDU1_16 + data_trump$EDU2_16 + data_trump$EDU3_16 + data_trump$EDU4_16
summary(data_trump$EDU_total_16)

summary(data_trump$SHARE_R)
summary(data_trump$SHARE_T)
summary(data_trump$SHARE_T_20)

data_trump$mymargin2016_cal <-  data_trump$SHARE_R - data_trump$SHARE_T
summary(data_trump$mymargin2016_cal)
summary(data_trump$MAR_T_new)

data_trump$mymargin2020_cal <-  data_trump$SHARE_R - data_trump$SHARE_T_20
summary(data_trump$mymargin2020_cal)
summary(data_trump$MAR_T_20)
summary(data_trump$MAR_T_20_new)


smodel16_5 <- lm(mymargin2020_cal ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + SC5_PCM_14 + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = data_trump)
summary(smodel16_5)






#################
data_trump2$mymargin2016_cal <-  data_trump2$SHARE_R - data_trump2$SHARE_T
summary(data_trump2$mymargin2016_cal)
summary(data_trump2$MAR_T_new)

data_trump2$mymargin2020_cal <-  data_trump2$SHARE_R - data_trump2$SHARE_T_20
summary(data_trump2$mymargin2020_cal)
summary(data_trump2$MAR_T_20)
summary(data_trump2$MAR_T_20_new)




# Civic Engagement
smodelnew16_1 <- data_trump2 %>% rename(
  social_community = civic_organizations_county
) %>% 
  lm(mymargin2016_cal ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + social_community + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = .)
summary(smodelnew16_1)

smodelnew16_2 <- data_trump2 %>% rename(
  social_community = volunteering_rate_county
) %>% 
  lm(mymargin2016_cal ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + social_community + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = .)
summary(smodelnew16_2)

# Social Cohesion
smodelnew16_3 <- data_trump2 %>% rename(
  social_community = clustering_county
) %>%
  lm(mymargin2016_cal ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + social_community + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = .)
summary(smodelnew16_3)

smodelnew16_4 <- data_trump2 %>% rename(
  social_community = support_ratio_county
) %>%
  lm(mymargin2016_cal ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + social_community + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = .)
summary(smodelnew16_4)

# Economic Connectedness
smodelnew16_5 <- data_trump2 %>% rename(
  social_community = ec_county
) %>%
  lm(mymargin2016_cal ~ INC_PC_16 + GINI_16 + EMP_AVE_CGR80_19 + social_community + POP_DEN_16 + UNE_16 + EDU1_16 + R_BLACK_16 + SEX_RATIO + AGE_DEP_YOUNG + MARRIED + factor(S_ID), data = .)
summary(smodelnew16_5)


################################

presdata <- read.csv("Data_SocCap/Presidential_Results_1216.csv")
presdata20 <- read.csv("Data_SocCap/2020countypres.csv")

data_trump2 <- merge(data_trump2, presdata, by.x = "FIPS_ID", by.y = "FIPS", all = FALSE)

data_trump2[1:10, c("SHARE_T", "per_gop_2016")]
data_trump2[1:10, c("SHARE_R", "per_gop_2012")]

data_trump2 <- merge(data_trump2, presdata20, by.x = "FIPS_ID", by.y = "county_fips", all = FALSE)
data_trump2[1:10, c("SHARE_T_20", "per_gop")]


######################
acs <- read.csv("Data_SocCap/ACSData.csv")


#######################
summary(data_trump$INC_PC_16)
summary(data_trump$GINI_16)
summary(data_trump$POP_DEN_16)
summary(data_trump$UNE_16)
summary(data_trump$EDU1_16)
summary(data_trump$R_BLACK_16)
summary(data_trump$SEX_RATIO)
summary(data_trump$AGE_DEP_YOUNG)
summary(data_trump$MARRIED)
summary(data_trump$SC5_PCM_14)
summary(data_trump$MAR_T_new)
summary(data_trump$MAR_T_20_new)



summary(data_trump$EMP_AVE_CGR80_19)
summary(data_trump$POP_CGR80_19)
summary(data_trump$EAR_AVE_JOB_CGR80_19)
summary(data_trump$WA_SA_MEA_CGR80_19)

summary(data_meta$ec_county)
summary(data_meta$support_ratio_county)
summary(data_meta$clustering_county)
summary(data_meta$volunteering_rate_county)
summary(data_meta$civic_organizations_county)

####### Regressions with trump votes


