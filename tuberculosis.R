library(tidyverse)

#######################
# inspect the dataset #
#######################
tidyr::who

##################
# clean the data #
##################
who <- tidyr::who %>%
  # transform column values into single column
  pivot_longer(new_sp_m014:newrel_f65, 
               names_to = "key", 
               values_to = "cases"
               ) %>% 
  # filter missing values
  filter(!is.na(cases)) %>%
  # fix "newrel" values to "new_rel"
  mutate(key = str_replace(key, "newrel", "new_rel")) %>%
  #separate the values in column "key"
  separate(key, c("new", "type", "sex_age"), sep = "_") %>%
  separate(sex_age, c("sex", "age"), sep = 1)

# inspect the "new" variable
who %>% count(new)

# drop redundant columns
who <- who %>% 
  select(-iso2, -iso3, -new)

##################
# Summarise data #
##################

# summarise data by country
who %>% group_by(country) %>%
  summarise(average_cases = round(mean(cases), 0)) %>%
  arrange(desc(average_cases))

# summarise data by gender
who %>% group_by(sex) %>%
  summarise(total_cases = round((sum(cases) / sum(.$cases) * 100), 0))

# summarise data by age
who %>% group_by(age) %>%
  summarise(average_cases = round(mean(cases), 0)) %>%
  arrange(average_cases)

#################################################
# plot average tuberculosis in top 10 countries #
#################################################
top_ten1 <- who %>%
  group_by(country) %>%
  summarise(average_cases = round(mean(cases), 0)) %>%
  ungroup() %>%
  arrange(desc(average_cases)) %>%
  top_n(10)

# Shorten country name for cleaner plot
top_ten2 <- top_ten1 %>%
  mutate(country = str_replace(country, 
                               "Democratic Republic of the Congo", 
                               "Congo"
                               )
         )

ggplot(data = top_ten2) +
  geom_col(aes(x = country, y = average_cases, fill = country))

#####################################################
# plot tuberculosis development in top 10 countries #
#####################################################

top_ten3 <- who %>%
  # limit the original dataset to top 10 countries
  semi_join(top_ten1, by = "country") %>%
  group_by(country, year) %>%
  # calculate average cases per year
  summarise(average_cases = round(mean(cases), 0)) %>%
  ungroup()

ggplot(data = top_ten3) +
  geom_smooth(aes(x = year, y = average_cases, color = country))

###########################################
# plot average tuberculosis in age groups #
###########################################
age_groups <- who %>%
  group_by(age) %>%
  summarise(average_cases = round(mean(cases), 0)) %>%
  arrange(average_cases)

ggplot(data = age_groups) +
  geom_col(aes(x = age, y = average_cases, fill = age)) +
  # add median line to the plot
  geom_abline(slope = 0, 
              intercept = median(age_groups$average_cases),
              color = "red",
              lty = 2)

############################
# Modelling age and gender #
############################

# Divide data to age groups
model_data <- who %>%
  filter(age != "014") %>%
  mutate(age_group = ifelse(age == "1524" | age == "2543" | age == "3544",
         "young", "old"))

# model the data
model1 <- lm(cases ~ age_group, data = model_data)
summary(model1)

# add gender to the model
model2 <- lm(cases ~ age_group + sex, data = model_data)
summary(model2)
