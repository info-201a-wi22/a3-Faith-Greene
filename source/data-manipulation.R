#downloads and saves dataset, commented out to save runtime
#to_save <- read.csv("https://raw.githubusercontent.com/vera-institute/"
#                     "incarceration-trends/master/incarceration_trends.csv")
#write.csv(to_save, "Incarseration-Trends.csv")
library("tidyverse")
library("ggplot2")
library("sf")
library("naniar")

incarserations <-
  read.csv("~/INFO_201/Code_info_201/a3-Faith-Greene/source/Incarseration-Trends.csv")

racial_data_no_gender <- select(incarserations,
                                c("aapi_prison_pop", "black_prison_pop",
                                  "latinx_prison_pop", "native_prison_pop",
                                  "white_prison_pop",
                                  "other_race_prison_pop")) %>%
  gather(key = race, value = pop) %>%
  na.omit() %>%
  group_by(race) %>%
  summarize(mean(pop, rm.na = TRUE))

racial_data_male <- select(incarserations,
                            c("aapi_male_prison_pop", "black_male_prison_pop",
                              "latinx_male_prison_pop",
                              "native_male_prison_pop",
                              "white_male_prison_pop",
                              "other_race_male_prison_pop"))


racial_data_female <- select(incarserations,
                           c("aapi_female_prison_pop",
                             "black_female_prison_pop",
                             "latinx_female_prison_pop",
                             "native_female_prison_pop",
                             "white_female_prison_pop",
                             "other_race_female_prison_pop"))
  

colnames(racial_data_male) <- c("aapi_prison_pop", "black_prison_pop",
                                "latinx_prison_pop", "native_prison_pop",
                                "white_prison_pop", "other_race_prison_pop")
colnames(racial_data_female) <- c("aapi_prison_pop", "black_prison_pop",
                                  "latinx_prison_pop", "native_prison_pop",
                                  "white_prison_pop", "other_race_prison_pop")
racial_data_male <- gather(racial_data_male, key = race, value = pop) %>%
  na.omit() %>%
  group_by(race) %>%
  summarize(mean(pop, rm.na = TRUE))

racial_data_female <- gather(racial_data_female, key = race, value = pop) %>%
  na.omit() %>%
  group_by(race) %>%
  summarize(mean(pop, rm.na = TRUE))
together <- left_join(racial_data_no_gender, racial_data_male, by = "race") %>%
  left_join(racial_data_female, by = "race")
colnames(together) <- c("race", "avg_total_pop", "avg_male_pop",
                        "avg_female_pop")
together_with_percentage <- mutate(together,
                                   percent_deviation_from_avg_male =
                                     ((avg_male_pop /
                                         avg_total_pop) - 1) * 100) %>%
  mutate(percent_deviation_from_avg_female =
           ((avg_female_pop / avg_total_pop) - 1) * 100)
rownames(together_with_percentage) <- c("Asian/Pacific Islanders",
                                        "African American", "Latinx",
                                        "Native American", "Other", "White")

non_prison_pop_stats <- select(incarserations, c("total_pop_15to64",
                                                 "female_pop_15to64",
                                                 "male_pop_15to64",
                                                 "aapi_pop_15to64",
                                                 "black_pop_15to64",
                                                 "latinx_pop_15to64",
                                                 "white_pop_15to64",
                                                 "native_pop_15to64")) %>%
  gather(key = demo, value = pop) %>%
  na.omit() %>%
  group_by(demo) %>%
  summarize(mean(pop, rm.na = TRUE))

together_with_percentage$total_non_prison_pop <- c(non_prison_pop_stats[[1, 2]],
                                                   non_prison_pop_stats[[2, 2]],
                     non_prison_pop_stats[[4, 2]], non_prison_pop_stats[[6, 2]],
                     NA,
                     non_prison_pop_stats[[7, 2]])
together_with_percentage <- mutate(together_with_percentage,
                                   percent_in_prison =
                                     avg_total_pop / total_non_prison_pop)

plot_1_filtered <- select(incarserations, c("year", "black_male_prison_pop",
                                            "black_female_prison_pop")) %>%
  na.omit() %>%
  gather(key = gender, value = population, -year) %>%
  group_by(year, gender) %>%
  summarize(pop = mean(population))
black_incarseration_over_time <- ggplot(plot_1_filtered) +
  geom_col(mapping = aes(x = year, y = pop, fill = gender)) +
  scale_fill_discrete(labels = c("Female", "Male")) +
  labs(title = "Average African Americans in Prison in a County, by Year",
       y = "Average of imprisoned African Americans in a county")

plot_2_filtered <- select(incarserations, c("black_male_prison_pop",
                                            "black_female_prison_pop",
                                            "black_pop_15to64")) %>%
  na.omit()

black_incarserations_compared_to_pop <- ggplot(plot_2_filtered) +
  geom_point(mapping = aes(x = black_pop_15to64,
                          y = black_male_prison_pop, color = "Male")) +
  geom_smooth(mapping = aes(x = black_pop_15to64,
                            y = black_male_prison_pop, color = "Male")) +
  geom_point(mapping = aes(x = black_pop_15to64,
                          y = black_female_prison_pop, color = "Female")) +
  geom_smooth(mapping = aes(x = black_pop_15to64,
                            y = black_female_prison_pop, color = "Female")) +
  labs(title = "African American County Prison Population by Total
       African American County Population",
       x = "African Amercian Population by County",
       y = "African American's in prison by County",
       color = "Gender")


county_map <- map_data("county") %>%
  mutate(location = str_c(subregion, ", ", region))
plot_3_filtered <- select(incarserations, c("state", "county_name",
                                            "black_prison_pop",
                                             "black_pop_15to64", "year")) %>%
  filter(year == 2012) %>%
  mutate(state = state.name[match(state, state.abb)]) %>%
  filter(state != "Alaska") %>%
  filter(state != "Hawaii") %>%
  mutate(county_name = str_replace(county_name, " County", "")) %>%
  mutate(county_name = str_replace(county_name, " Parish", "")) %>%
  mutate(county_name = str_replace(county_name, " Census Area", "")) %>%
  mutate(location = str_c(tolower(county_name), ", ", tolower(state)))
plot_3_filtered[is.na(plot_3_filtered)] <- -1
plot_3_filtered$Percentage <- na_if((plot_3_filtered$black_prison_pop), -1) /
  (na_if(plot_3_filtered$black_pop_15to64, -1)) * 100
to_map <- left_join(county_map, plot_3_filtered, by = "location")
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )
map <- ggplot(to_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = Percentage)) +
  coord_map() +
  labs(title = "2012 Percentage of African American Population ages 15 to
       64 imprisoned by county",
       fill = "Percentage in Prison (Grey means NA)") +
  blank_theme
