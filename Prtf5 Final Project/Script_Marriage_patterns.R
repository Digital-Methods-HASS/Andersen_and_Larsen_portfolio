

#install packages 
install.packages("tidyverse")
library(tidyverse)

civil_status<-read_csv2("data/Civil_status2.csv")
glimpse(civil_status)

Labor_force_women<-read_csv2("data/Labor_force2.csv")
glimpse(Labor_force_women)

Birth_rates<-read_csv2("data/Birth_rates.csv")

colnames(civil_status)
colnames(Labor_force_women)


#1 Visualization of No. of women by civil status pr. year from 1971 to 2025
civil_status %>%
  filter(Civil_status %in% c("Married", "Unmarried")) %>%
  group_by(Year, Civil_status) %>%
  summarise(Number = sum(Number, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Number, color = Civil_status)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "No. of women by civil status pr. year (1971-2025)",
    x = "Year",
    y = "No. of women in the age of 18-35 years",
    color = "Civil_status"
  ) +
  theme_minimal()


#2 Visualization of women's age of marriage
civil_status %>%
  filter(
    Civil_status == "Married",
    Age >= 18,
    Age <= 35,
    Year >= 1971,
    Year <= 2025
  ) %>%
  
  mutate(
    Age_group = case_when(
      Age >= 18 & Age <= 20 ~ "18-20",
      Age >= 21 & Age <= 24 ~ "21-24",
      Age >= 25 & Age <= 28 ~ "25-28",
      Age >= 29 & Age <= 32 ~ "29-32",
      Age >= 33 & Age <= 35 ~ "33-35"
    )
  ) %>%
  
  group_by(Year, Age_group) %>%
  summarise(
    Number = sum(Number),
    .groups = "drop"
  ) %>%
  
  ggplot(aes(x = Year, y = Number, color = Age_group)) +
  geom_line(linewidth = 1.3) +
  labs(
    title = "Development in marriage age groups, 1971-2025",
    x = "Year",
    y = "Number of women",
    color = "Age group"
  ) +
  facet_wrap(~Age_group) +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )



#3 Visualization of the development in women's average age of marriage compared to women's labor force participation

# Civil status
civil_status_graph <- civil_status %>%
  filter(
    Civil_status == "Married",
    Year >= 1971,
    Year <= 1991
  ) %>%
  group_by(Year) %>%
  summarise(
    Value = weighted.mean(Age, Number),
    .groups = "drop"
  ) %>%
  mutate(
    Group = "Average age of Marriage"
  )

# Labor force
labor_force_graph <- Labor_force_women %>%
  filter(
    Year >= 1971,
    Year <= 1991
  ) %>%
  transmute(
    Year,
    Value = Women_Pct.,
    Group = "Women in Labor force (%)"
  )

# Combine data
graph_data <- bind_rows(civil_status_graph, labor_force_graph)

# Create index
graph_data <- graph_data %>%
  group_by(Group) %>%
  mutate(
    Index = Value / first(Value) * 100
  ) %>%
  ungroup()

# Create visualization
ggplot(graph_data, aes(x = Year, y = Index, color = Group)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  geom_text(
    data = graph_data %>%
      filter(Year %% 2 == 1),
    aes(label = round(Value, 1)),
    color = "black",
    vjust = -0.7,
    size = 2.5,
    show.legend = FALSE
  ) +
  labs(
    title = "Development in age of marriage and women's labor force participation",
    x = "Year",
    y = "Index (1971 = 100)",
    color = "Group"
  ) +
  theme_minimal()



#4 Visualization of the development in age of marriage and age at childbirth 1970-2025

# Average age of marriage
civil_status_graph <- civil_status %>%
  filter(
    Civil_status == "Married",
    Year >= 1970,
    Year <= 2025
  ) %>%
  group_by(Year) %>%
  summarise(
    Value = weighted.mean(Age, Number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Group = "Average age of Marriage"
  )

# Average age at childbirth
births_graph <- Birth_rates %>%
  filter(
    Year >= 1970,
    Year <= 2025
  ) %>%
  mutate(
    age_birth = readr::parse_number(Age_group),
    Births = readr::parse_number(as.character(Births))
  ) %>%
  filter(
    !is.na(age_birth),
    !is.na(Births)
  ) %>%
  group_by(Year) %>%
  summarise(
    Value = weighted.mean(age_birth, Births, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    Year = Year,
    Value,
    Group = "Average age at Birth"
  )

# Combine data
graph_data <- bind_rows(civil_status_graph, births_graph)

# Create visualization
ggplot(graph_data, aes(x = Year, y = Value, color = Group)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  geom_text(
    data = graph_data %>%
      filter(Year %% 5 == 0),
    aes(label = round(Value, 1)),
    color = "black",
    vjust = -0.7,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Development in age of marriage and age at childbirth 1970-2025",
    x = "Year",
    y = "Average age",
    color = "Group"
  ) +
  theme_minimal()

