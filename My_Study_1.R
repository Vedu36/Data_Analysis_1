#importing Library

install.packages("tidyverse")
library(tidyverse)

install.packages("patchwork")
library(patchwork)

#importing data

data_1 <- read.csv("C:/Users/vedag/Documents/case_study/My_Study/dailyActivity_merged.csv")

#viewing data
View(data_1)
str(data_1) #gives all the details
glimpse(data_1)
colnames(data_1)



#Data cleaning

#Transforming "ActivityDate" in to data format
data_1$ActivityDate <- as.Date(data_1$ActivityDate, format = "%m/%d/%y")
str(data_1)
#Cleaning the columns names 
names(data_1) <- gsub("([a-z])([A-Z])", "\\1_\\2", names(data_1), perl = TRUE)

#Checking Total_Distance and Tracker_Distanceare equal 
First_filter <- filter(data_1,Total_Distance!=Tracker_Distance)
head(First_filter)

#Creating a column for printing week days
data_1$Day_Of_Week <- weekdays(data_1$Activity_Date)
str(data_1)
view(data_1)

# Create a vector of day names in the correct order
day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Create a new column "DayOfWeekNumeric"
data_1$Day_Of_Week_Numeric <- match(weekdays(data_1$Activity_Date), day_names) - 1

#Creating a subset of data
colnames(data_1)
data_sub <- data_1[,c("Id","Activity_Date","Total_Steps","Total_Distance",
                        "Very_Active_Minutes","Fairly_Active_Minutes",
                        "Lightly_Active_Minutes","Sedentary_Minutes","Calories",
                        "Day_Of_Week","Day_Of_Week_Numeric"
)]
str(data_sub)

head(data_sub)


#Entering Analyze the Data Phase

#Category_1

# Group by "Id" and calculate averages
averages_by_step <- data_sub %>%
  group_by(Id) %>%
  summarise(Avg_Steps = mean(Total_Steps))


# Ploting a graph
ggplot(data_sub, aes(x = Id, y = Total_Steps)) +
  geom_point() +
  labs(x = "Id", y = "Total Steps", title = "Scatter Plot of Total Steps by Id")

#Adding 3 catagories

averages_by_step <- averages_by_step %>%
  mutate(Step_Category = case_when(
    Avg_Steps <= 6000 ~ "Sedentary_Active",
    Avg_Steps > 6000 & Avg_Steps < 12000 ~ "Active",
    Avg_Steps >= 12000 ~ "Very_Active",
  ))

# Create a lookup table with Id and Step_Category
lookup_table <- averages_by_step %>%
  select(Id, Step_Category)

# Join the lookup table with data_sub
data_sub <- data_sub %>%
  left_join(lookup_table, by = "Id")

summary(data_sub)


#Finding Correlation bw columns "Total_Steps" and "Calories"
correlation <- cor(data_sub$Total_Steps, data_sub$Calories)

# Display the correlation coefficient
print(correlation)

#Ploting graph bw columns "Total_Steps" and "Calories"
ggplot(data_sub, aes(x = Total_Steps, y = Calories)) +
  geom_point() +
  labs(x = "Total Steps", y = "Calories", title = "Scatter Plot: Total Steps vs. Calories")
#add hue 
ggplot(data_sub, aes(x = Total_Steps, y = Calories, color = Step_Category)) +
  geom_point() +
  labs(x = "Total Steps", y = "Calories", title = "Scatter Plot: Total Steps vs. Calories") +
  scale_color_manual(values = c("Sedentary_Active" = "blue", "Active" = "green", "Very_Active" = "red"))


days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")


ggplot(data_sub, aes(x = factor(Day_Of_Week,levels = days_order), y = Total_Steps)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Day of Week", y = "Average Steps", title = "Average Steps per Day")

# Correlation bw steps and distance
correlation_1 <- cor(data_sub$Total_Steps, data_sub$Total_Distance)
print(correlation_1)



activity_sum <- data_sub %>%
  summarise(
    Very_Active_Minutes_Sum = sum(Very_Active_Minutes),
    Fairly_Active_Minutes_Sum = sum(Fairly_Active_Minutes),
    Lightly_Active_Minutes_Sum = sum(Lightly_Active_Minutes),
    Sedentary_Minutes_Sum = sum(Sedentary_Minutes)
  )



# activity_sum contains summed activity minutes (Pie chart)
activity_sum %>%
  pivot_longer(
    cols = c("Very_Active_Minutes_Sum", "Fairly_Active_Minutes_Sum", "Lightly_Active_Minutes_Sum", "Sedentary_Minutes_Sum"),
    names_to = "Activity_Type",
    values_to = "Minutes_Sum"
  ) %>%
  ggplot(aes(x = "", y = Minutes_Sum, fill = Activity_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Activity Type", title = "Distribution of Activity Minutes") +
  theme_void() +
  geom_text(aes(label = paste0(round((Minutes_Sum / sum(Minutes_Sum)) * 100, 1), "%")), position = position_stack(vjust = 0.5))

data_sub <- data_sub %>% 
  mutate(Total_min = rowSums(select(., "Very_Active_Minutes",
                                    "Fairly_Active_Minutes",
                                    "Lightly_Active_Minutes",
                                    "Sedentary_Minutes")))

correlation_2 <- cor(data_sub$Calories,data_sub$Total_min)
print(correlation_2)

correlation_3 <- cor(data_sub$Total_Steps,data_sub$Total_min)
print(correlation_3)

# Create scatter plots for different pairs of variables
scatter_plot_1 <- ggplot(data_sub, aes(x = Calories, y = Sedentary_Minutes, color = Step_Category)) +
  geom_point() +
  labs(title = "Sedentary Minutes")+
  scale_color_manual(values = c("Sedentary_Active" = "blue", "Active" = "green", "Very_Active" = "red"))

scatter_plot_2 <- ggplot(data_sub, aes(x = Calories, y = Lightly_Active_Minutes, color = Step_Category)) +
  geom_point() +
  labs(title = "Lightly Active Minutes")+
  scale_color_manual(values = c("Sedentary_Active" = "blue", "Active" = "green", "Very_Active" = "red"))

scatter_plot_3 <- ggplot(data_sub, aes(x = Calories, y = Fairly_Active_Minutes, color = Step_Category)) +
  geom_point() +
  labs(title = "Fairly Active Minutes") + 
  scale_color_manual(values = c("Sedentary_Active" = "blue", "Active" = "green", "Very_Active" = "red"))

scatter_plot_4 <- ggplot(data_sub, aes(x = Calories, y = Very_Active_Minutes,color = Step_Category)) +
  geom_point() +
  labs(title = "Very Active Minutes") +
  scale_color_manual(values = c("Sedentary_Active" = "blue", "Active" = "green", "Very_Active" = "red"))

# Arrange the scatter plots in a 2x2 grid
multiplot <- (scatter_plot_1 | scatter_plot_2) / (scatter_plot_3 | scatter_plot_4)


# Display the multiplot
multiplot

