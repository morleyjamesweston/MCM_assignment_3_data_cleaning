# █████╗ ███████╗███████╗██╗ ██████╗ ███╗   ██╗███╗   ███╗███████╗███╗   ██╗████████╗    ██████╗ 
# ██╔══██╗██╔════╝██╔════╝██║██╔════╝ ████╗  ██║████╗ ████║██╔════╝████╗  ██║╚══██╔══╝    ╚════██╗
# ███████║███████╗███████╗██║██║  ███╗██╔██╗ ██║██╔████╔██║█████╗  ██╔██╗ ██║   ██║        █████╔╝
# ██╔══██║╚════██║╚════██║██║██║   ██║██║╚██╗██║██║╚██╔╝██║██╔══╝  ██║╚██╗██║   ██║        ╚═══██╗
# ██║  ██║███████║███████║██║╚██████╔╝██║ ╚████║██║ ╚═╝ ██║███████╗██║ ╚████║   ██║       ██████╔╝
# ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝   ╚═╝       ╚═════╝ 

student_name <- "Jürgen Habermas"

# This week's assignment is very open-ended. You'll want to do the following:


# 1. Find a statistical data set of your choice, and download it in your code using `download.file()`

# An open data repository is often a good starting point.
# Some data repositories I've used in previous research include:

#   https://data.europa.eu/en
#   https://opendata.swiss/de
#   https://data.gov.tw/
#   https://databank.worldbank.org/
#   https://data.gov/
#   https://data.egov.kz/
#   https://www.data.gov.uk/
#   https://data.go.id/
#   https://egypt.opendataforafrica.org/
#   https://data.un.org/

# 2. Clean the data set and use summarise() or count() to provide 3 descriptive statistics, 
#    such as the average X, or the total Y. Print the statistic in a full sentence.

# 3. Save the cleaned data set in the .rds format.

# 4. Use the data set to produce 2 charts using ggplot().










# ███████╗██╗  ██╗ █████╗ ███╗   ███╗██████╗ ██╗     ███████╗
# ██╔════╝╚██╗██╔╝██╔══██╗████╗ ████║██╔══██╗██║     ██╔════╝
# █████╗   ╚███╔╝ ███████║██╔████╔██║██████╔╝██║     █████╗  
# ██╔══╝   ██╔██╗ ██╔══██║██║╚██╔╝██║██╔═══╝ ██║     ██╔══╝  
# ███████╗██╔╝ ██╗██║  ██║██║ ╚═╝ ██║██║     ███████╗███████╗
# ╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝     ╚══════╝╚══════╝

# Here's an example: (You can't use this one)

library(tidyverse)
library(readxl)
library(glue)

# Houseless and Institutional Households and their Population, from the Indian census bureau.
download.file(
  "https://censusindia.gov.in/nada/index.php/catalog/45262/download/48988/PCA11-A05APPX-0000.xlsx",
  "indian_homeless.xlsx"
  )

indian_homeless <- read_excel("indian_homeless.xlsx", skip = 3)
indian_homeless <- indian_homeless |> select(1:8) # I only need the first 8 columns for this data set


# The column names are a mess, so I just replaced them all.
colnames(indian_homeless) <- 
  c("id", "state_type", "state", "urbanity", "household", "persons", "male", "female")

# In keeping with Tidy Data principles, I pivoted the data frame wider, so that each column represented a different kind of observation
indian_homeless <- 
  indian_homeless |> 
  pivot_wider(names_from = urbanity, values_from = c(household, persons, male, female))

# Some states were lower case, some were upper. I also removed the totals.
indian_homeless <- indian_homeless |> 
  mutate(state = toupper(state)) |> 
  filter(state != "INDIA")


# However, I also wanted the population of each state, so I found another data set.

# Population of India, from the Indian census bureau.
download.file(
  "https://censusindia.gov.in/nada/index.php/catalog/42526/download/46152/A-1_NO_OF_VILLAGES_TOWNS_HOUSEHOLDS_POPULATION_AND_AREA.xlsx",
  "indian_population.xlsx"
)
indian_population <- read_excel("indian_population.xlsx", 
                                col_names = FALSE, skip = 4)

# I only kept the columns I needed for this.
indian_population <- indian_population |> 
  select(c(4, 5, 6, 11))
colnames(indian_population) <- c("state_type", "state", "urbanity", "population")

indian_population <- indian_population |> 
  filter(state_type == "STATE") |> # I only kept the state data.
  filter(urbanity == "Total") |>  # For this, I didn't worry about male and female
  select(state, population) |> # I only kept the columns I needed
  mutate(state = str_replace_all(state, " @&", "")) |> # a mysterious @& was present in one of the state names.
  mutate(state = str_replace(state, "UTTARAKHAND", "UTTRAKHAND")) # One state was spelled differently in the two data sets.

# We will learn about joining in a few weeks, but this just combines the two data sets into one
indian_homeless <- indian_homeless |> left_join(indian_population)

# I made a new column for per-capita homeless population
indian_homeless <- indian_homeless |> 
  mutate(homeless_per_capita = persons_Total / population)

# This gives me the sum of the relevant column
homeless_population <- indian_homeless |> 
  summarise(population = sum(persons_Total))
glue("There are {homeless_population} homeless people in India.")

# arrange() and head(1) are an easy way to select the maximum
max_homeless_per_capita <- indian_homeless |> 
  arrange(desc(homeless_per_capita)) |> 
  head(1) |> 
  select(state)
glue("{max_homeless_per_capita} Has the most homeless people per capita.")

male_female_ratio <- indian_homeless |> 
  mutate(male_female_ratio = male_Total / female_Total) |> 
  select(state, male_female_ratio) |> 
  arrange(male_female_ratio)
# You can even perform these operations inside other statements
glue("{male_female_ratio |> head(1) |> select(state)} has the most homeless men compared to women, whereas {male_female_ratio |> tail(1) |> select(state)} has the highest ratio of homeless women.")

# Plot 1: Percent homeless by state
indian_homeless |> 
  arrange(homeless_per_capita) |> 
  mutate(state = factor(state, levels = state)) |> # We will learn about this on week 10, but this keeps ggplot from changing the order of the states.
  ggplot(aes(x=homeless_per_capita * 100, y=state)) + # multiply by 100 to get percentages
  geom_col() +
  labs(x="Population", y="State", title="Indian states by percent homeless")

# Plot 2: Total homeless by population, men and women.
indian_homeless |>
  select(male_Total, female_Total, state, population) |> 
  # Now, we want to compare men and women, so we should consider them as the same kind of data, and once again pivot longer.
  pivot_longer(cols=c(male_Total, female_Total), names_to = "Gender", values_to = "Homeless_population") |>
  ggplot(aes(x=population, y= Homeless_population, color=state, shape=Gender)) +
  geom_point(size = 5) + # makes the points big.
  labs(x="Total state population", y="Homeless population per capita", title="Homeless men and women in different Indian states") +
  theme_bw()
  



