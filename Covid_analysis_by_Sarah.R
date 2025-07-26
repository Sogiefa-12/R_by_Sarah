# Loading the Dataset
data <- read.csv("C:/Users/user/Desktop/R_by_Sarah/R_by_Sarah/covid_19_data.csv")
# Understanding the Data
## Loading the dataset from the `covid19.csv` CSV file and quick exploration
library(readr)

#Displacing the dimension of the data
dim(data)

#Storing the cvariableolumn names in a 
vector_cols <- colnames(data)

# Dispaying the variable vector_cols
vector_cols

# showing the first few rows of the dataset
head(data)

# showing a global view of the dataset
library(tibble)
glimpse(data)  

#1. This variable contains a character vector.

#2. The use of the function `glimpse()` is the very first operation to do because we don't only learn about the dimensions of the database but also about the names of the first columns and their types and content. It can replace the three previous operations: `dim()`, `colnames()`, and `head()`.

# DATA CLEANING
# Isolating the Rows we Need
library(dplyr) #loading the dplyr library

# Filter the "All States" Province states and remove the "Province_State" colume, replacing it with "All States"
covid_df_all_states <- data %>%
  filter(Province.State=="All States") %>%
  select(-Province.State)

# - We can remove `Province_State` without loosing information because after the filtering step this column only contains the value `"All States"`.

# Selecting the columns with cumulative numbers
covid_df_all_states_data <- data %>%
 select(ObservationDate, Country.Region, Confirmed, Deaths, Recovered)  
head(covid_df_all_states_data) # to Display 6 Rows
view(covid_df_all_states_data) # to display it in a new tab has a table

# Extracting The Top Ten Countries in the Number of Deah Cases
# Summarizing the data based on the 'Country.Region' Coulumn

covid_df_all_states_data_sum <- covid_df_all_states_data %>%
  group_by(Country.Region) %>%
  summarise(confirmed = sum(Confirmed),
            deaths = sum(Deaths),
            recovered = sum(Recovered)) %>%
  arrange(desc(deaths)) # this is equivalent to arrange(-deaths)

covid_df_all_states_data_sum
view(covid_df_all_states_data_sum) # viewing it in a tubular form

# Taking The Top 10 Countries

covid_top_10 <- head(covid_df_all_states_data_sum, 10)
covid_top_10


# Identifying The Highest Confirmed Cases Against Recovered Cases
## Getting Vectors

countries <- covid_top_10$Country.Region
confirmed_cases <- covid_top_10$confirmed
death_cases <- covid_top_10$deaths
recovered_cases <- covid_top_10$recovered


## Visualization of the Top 10 Countries with the Highest Covid19 Deaths

library(ggplot2)

ggplot(covid_top_10, aes(x = reorder(countries, death_cases), y= death_cases, fill = countries)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Countries of Covid Death Cases",
       x = "Countries",
       y = "Death Cases",
       fill = "Countries") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Naming Vectors

names(confirmed_cases) <- countries
names(death_cases) <- countries
names(recovered_cases) <- countries

## Visualization of the Top 10 Countries with the Highest Confirmed Cases

ggplot(covid_top_10, aes(x = reorder(countries, confirmed_cases), y = confirmed_cases, fill = countries)) +
  geom_point(size = 3, shape = 21) +
  geom_segment(aes(x = reorder(countries, confirmed_cases), xend = countries, y = 0, yend = confirmed_cases)) +
  labs(title = "Top 10 Countries of Covid Confirmed Cases",
       x = "Countries",
       y = "Confirmed Cases") +
  theme_minimal()


# Sort the data by recovered_cases
covid_top_10 <- covid_top_10[order(recovered_cases, decreasing = TRUE), ]

# Create a lollipop chart
ggplot(covid_top_10, aes(x = reorder(countries, recovered_cases), y = recovered_cases, color = countries)) +
  geom_point(size = 4, shape = 21) +
  geom_segment(aes(x = countries, xend = reorder(countries, recovered_cases), y = 0, yend = recovered_cases), color = "grey") +
  coord_flip() +
  labs(title = "Top 10 Countries of Covid Recovered Cases",
       x = "Countries",
       y = "Recovered Cases") +
  theme_minimal() +
  theme(panel.grid = element_blank())

## Identifying

death_cases
sum(death_cases) # finding the total sum of death cases
mean(death_cases) # the mean total
death_cases/sum(death_cases) # Normalizing the data to understand its proportion

