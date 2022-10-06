library(tidyverse)
library(janitor)
library(DataExplorer)
library(GGally)
library(here)

here()
# Read the data and clean names
menu_data <- read_csv("./data/menu.csv") %>%
  clean_names()

# Glimpse data
glimpse(menu_data)

# Create a general report to see if anything jumps out
create_report(menu_data,
              output_file = "menu_EDA.html",
              output_dir = "EDA_reports/")

# Calories by category
cal_by_cat <- menu_data %>%
  group_by(category) %>%
    summarise(avg = mean(calories),
              stdev = sd(calories))
ggplot(cal_by_cat, aes(x = category, y = avg)) +
  geom_pointrange(aes(ymin = avg - stdev, ymax = avg + stdev)) +
  geom_point()
names(menu_data)

# Create pairs plots excluding items and serving_size
g <- ggpairs(menu_data, columns = c(
  "category", "calories",
  "calories_from_fat", "total_fat",
  "total_fat_percent_daily_value", "saturated_fat",
  "saturated_fat_percent_daily_value", "trans_fat",
  "cholesterol", "cholesterol_percent_daily_value",
  "sodium", "sodium_percent_daily_value",
  "carbohydrates", "carbohydrates_percent_daily_value",
  "dietary_fiber", "dietary_fiber_percent_daily_value",
  "sugars", "protein",
  "vitamin_a_percent_daily_value", "vitamin_c_percent_daily_value",
  "calcium_percent_daily_value", "iron_percent_daily_value"
))
g

# Create pairs plots excluding items, serving_size, and percent_daily_value
h <- ggpairs(menu_data, columns = c(
  "category", "calories", "calories_from_fat",
  "total_fat", "saturated_fat", "trans_fat",
  "cholesterol", "sodium", "carbohydrates",
  "dietary_fiber", "sugars", "protein"
))
h

# Compare sugar to calories

sug_cal_comp <-  menu_data %>%
  select(item, calories, sugars)%>%
  group_by(item)

sug_cal_plot <- sug_cal_comp %>%
  ggplot(aes(calories, sugars))+
  geom_point()+
  ggtitle("Sugar vs. Calories")

sug_cal_plot

# Compare carbohydrates to calories

carb_cal_comp <-  menu_data %>%
  select(item, calories, carbohydrates)%>%
  group_by(item)

carb_cal_plot <- carb_cal_comp %>%
  ggplot(aes(calories, carbohydrates))+
  geom_point()+
  ggtitle("Carbohydrates vs. Calories")

carb_cal_plot

# Items with the highest cholesterol

cholesterol_levels <- menu_data %>%
  select(category, item, cholesterol) %>%
  group_by(category)

highest_cholesterol <- cholesterol_levels %>%
  arrange(desc(cholesterol))%>%
  head(10) %>% ggplot(aes(cholesterol, reorder(item, cholesterol), fill = item)) +
  geom_col() +
  geom_text(aes(label = cholesterol), position=position_stack(vjust=0.5),color="black",size=3) +
  ggtitle("McDonald's Top 10 Item with Highest Cholesterol Content") +
  theme(legend.position="none")

highest_cholesterol

# Items with the highest carbohydrates

carbohydrates_levels <- menu_data %>%
  select(category, item, carbohydrates) %>%
  group_by(category)

highest_carbohydrates <- carbohydrates_levels %>%
  arrange(desc(carbohydrates))%>%
  head(10) %>% ggplot(aes(carbohydrates, reorder(item, carbohydrates), fill = item)) +
  geom_col()+
  geom_text(aes(label = carbohydrates), position=position_stack(vjust=0.5),color="black",size=3)+
  ggtitle("McDonald's Top 10 Item with Highest Carbohydrates Content") +
  theme(legend.position="none")

highest_carbohydrates

# Items with the highest sugar

sugar_levels <- menu_data %>%
  select(category, item, sugars) %>%
  group_by(category)

highest_sugar <- sugar_levels %>%
  arrange(desc(sugars))%>%
  head(10) %>% ggplot(aes(sugars, reorder(item, sugars), fill = item)) +
  geom_col()+
  geom_text(aes(label = sugars), position=position_stack(vjust=0.5),color="black",size=3)+
  ggtitle("McDonald's Top 10 Item with Highest Sugar Content") +
  theme(legend.position="none")

highest_sugar





