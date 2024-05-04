#General Exploratory analysis
#
cities <- c('amsterdam', 'athens', 'barcelona', 'berlin', 'budapest', 'lisbon', 
            'london', 'paris', 'rome', 'vienna')
days <- c('weekdays', 'weekends')
#
k <- 0
for (city in cities) {
  for (day in days) {
    file_name <- paste0("~/Airbnb data/", city, "_", day, ".csv")
    data <- read.csv(file_name)
    # Remove the 'X' column if it exists
    if ("X" %in% colnames(data)) {
      data <- data[, -which(names(data) == "X")]
    }
    data <- data %>% mutate(city = city, weekend = day)
    if (k == 0) {
      all_cities <- data
      k <- 1
    } else {
      all_cities <- bind_rows(all_cities, data)
    }
  }
}
#Determining the shape of the dataset
dim(all_cities)
str(all_cities)

#character variables to factor
all_cities[, sapply(all_cities, is.character)] <- lapply(all_cities[, sapply(all_cities, is.character)], factor)


# Subset the all_cities dataset to include only categorical variables
cat_vars <- all_cities[, sapply(all_cities, is.factor)]

# Loop over each categorical variable and calculate frequency and percentage
stats_list <- list()
for (var in colnames(cat_vars)) {
  var_table <- table(cat_vars[, var])
  var_freq <- prop.table(var_table) * 100
  
  # Store the results in a list
  stats_list[[var]] <- data.frame(Category = names(var_table),
                                  Frequency = as.numeric(var_table),
                                  Percentage = round(var_freq, 2))
}

# Combine the results into a single data frame
stats_df <- do.call(rbind, stats_list)
# Display the results
stats_df[,-3]

#Exploratoty analysis
# Cbar of mean price by city
all_cities %>%
  group_by(city) %>%
  summarize(mean_price = mean(realSum)) %>%
  arrange(desc(mean_price)) %>% # Sort the data frame in descending order
  ggplot(aes(x = reorder(city, mean_price), y = mean_price)) +
  geom_bar(stat = "identity", fill = "#ADD8E6", color = "black") +
  labs(title = "Mean Price by City", x = "City", y = "Price (in euros)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0("$",round(mean_price, 2))), vjust = 0.5,hjust =1.3 , color = "black") +
  coord_flip()
#Cbar of mean satisfactory rating by city
#
mean_ratings <- aggregate(all_cities$guest_satisfaction_overall, 
                          by = list(city = all_cities$city), 
                          FUN = mean)
ggplot(data = mean_ratings, aes(x = city, y = x)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Mean Satisfactory Rating by City",
       x = "City",
       y = "Mean Satisfactory Rating") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# Percentage of listings on weekends
weekend_counts <- all_cities %>% 
  group_by(weekend) %>% 
  summarize(count = n()) %>% 
  mutate(percent = count/sum(count)*100)

# Donut chart using ggplot

ggplot(weekend_counts, aes(x = "", y = percent, fill = weekend)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +scale_fill_brewer(palette = "Pastel1") +theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0(round(percent, 2), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Listings by Weekend", fill = "Weekend")
#
# Percentage of listings by room type
room_type_counts <- all_cities %>% 
  group_by(room_type) %>% 
  summarize(count = n()) %>% 
  mutate(percent = count / sum(count) * 100)
custom_colors <- c("lightgreen", "lightblue", "darkred")

ggplot(room_type_counts, aes(x = "", y = percent, fill = room_type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(round(percent, 2), "%")), position = position_stack(vjust = 0.5)) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Distribution of Listings by Room Type") +
  labs(fill = "Room Type") +
  scale_fill_manual(values = custom_colors)

# Mean person capacity by room_type
mean_cap_by_room <- all_cities %>%
  group_by(room_type) %>%
  summarize(mean_cap = mean(person_capacity))

# Bar plot
ggplot(mean_cap_by_room, aes(x = "", y = mean_cap, fill = room_type)) +
  geom_bar(stat = "identity", width = 1, color = "darkgray") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0(round(mean_cap), " people")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Listings by Room Type", fill = "Room Type", y = "Mean Person Capacity")
#shared room has the highest mean capacity though it was the least among room types in all cities
#
#Average distance from center and from metro of different city
city_distances <- all_cities %>%
  group_by(city) %>%
  summarize(avg_dist = mean(dist),
            avg_metro_dist = mean(metro_dist))
# Reshape data for grouped bar plot
city_distances_long <- city_distances %>%
  pivot_longer(cols = c(avg_dist, avg_metro_dist),
               names_to = "distance_type",
               values_to = "average_distance")

# Create grouped bar plot
ggplot(city_distances_long, aes(x = city, y = average_distance, fill = distance_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_text(aes(label = paste0(round(average_distance, 1), " km")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Average Distance and Metro Distance by City", 
       x = "City", y = "Distance (km)", fill = "Distance Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("lightgreen", "#F5A9BC")) +
  guides(fill = guide_legend(title = "Distance Type"))
# Calculate the average cleanliness rating by room type
cleanliness_by_room <- all_cities %>%
  group_by(room_type) %>%
  summarize(avg_cleanliness = mean(cleanliness_rating))
#Ridgeline plot for visualization of the distribution rate
library(ggridges)
ggplot(all_cities, aes(x = cleanliness_rating, y = room_type, fill = room_type)) +
  geom_density_ridges(alpha = 0.7, scale = 3) +
  labs(title = "Distribution of Cleanliness Ratings by Room Type",
       x = "Density",
       y = "Room Type") +
  theme_minimal() +
  theme(legend.position = "none")


#Dividing data between high-scoresVSlow-scores
all_cities <- all_cities %>%
  mutate(satisfaction_category = ifelse(guest_satisfaction_overall > 75, "Good", "Bad"))
#
satisfaction_counts <- all_cities %>%
  group_by(weekend, satisfaction_category) %>%
  summarize(count = n(), .groups = 'drop')

# Plotting the bar graph
ggplot(satisfaction_counts, aes(x = satisfaction_category, y = count, fill = satisfaction_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Overall Guest Satisfaction", x = "Satisfaction Category", y = "Count", fill = "Satisfaction Category") +
  facet_wrap(~ weekend) +
  theme_minimal()
#Scores categorized as "Bad" during both weekdays and weekends are notably low and nearly identical. Thus, the availability of the Airbnb throughout the week does not correlate with low guest satisfaction.
#Are superhosts more likely to receive higher guest satisfaction scores?
# Host VS Superhost compariso
# Define a function to categorize hosts
SvsR <- function(host) {
  if (host == TRUE) {
    return('Superhost')
  } else if (host == FALSE) {
    return('Regular Host')
  } else {
    return('Unknown')
  }
}

# Apply the function to mutate the column
all_cities$Superhost <- sapply(all_cities$host_is_superhost, SvsR)

# Group the data by 'Superhost' and 'Good/Bad Score', count the occurrences, and unstack the result
HostGvBGrouped <- table(all_cities$Superhost, all_cities$satisfaction_category)
# Plot the grouped bar graph
barplot(HostGvBGrouped, beside = TRUE, col = c("darkred", "lightgreen"), 
        main = "Guest Satisfaction when the Host is/is not a Superhost",
        xlab = "Superhost", ylab = "Count", legend.text = TRUE)
#Is there a relationship between cleanliness ratings and guest satisfaction?
# data type check
class(all_cities$guest_satisfaction_overall)
class(all_cities$cleanliness_rating)
head(all_cities$guest_satisfaction_overall)
head(all_cities$cleanliness_rating)
#
library(ggplot2)

ggplot(all_cities, aes(x = guest_satisfaction_overall, y = cleanliness_rating)) +
  geom_point(color = "#008080", alpha = 0.4) +
  labs(title = "Overall Guest Satisfaction vs Cleanliness Rating",
       x = "Overall Guest Satisfaction", y = "Cleanliness Rating")
#Based on the plot, many listings with high scores have cleanliness ratings of 6 or higher. However, for guest satisfaction scores below 75, there is a wide range of cleanliness ratings. Interestingly, some listings with cleanliness ratings above 6 received low guest satisfaction scores. This suggests that while cleanliness is significant in the industry, it might not be the sole factor contributing to low satisfaction scores.
#
#Guest satisfaction correlated with proximity to metro stations?
ggplot(all_cities, aes(x = `guest_satisfaction_overall`, y = `metro_dist`)) +
  geom_point(alpha = 0.4, color = "slategrey") +
  labs(title = "Overall Guest Satisfaction vs Distance from Metro (km)",
       x = "guest_satisfaction_overall",
       y = "metro_dist (km)")
#This plot indicates that the majority of listings are typically located within 6 km of the metro, with many of them receiving high satisfaction scores. Although there are some outliers suggesting that a greater distance from the metro might result in higher guest satisfaction, these instances are too infrequent to draw definitive conclusions. Additionally, for listings within 6 km of the metro, particularly those within 2 km, other factors may be influencing their lower satisfaction scores
#
#Is there a relationship between the attraction index and guest satisfaction?
# Plotting the correlation using a scatter plot
plot(all_cities$guest_satisfaction_overall, all_cities$dist,
     xlab = "Overall Guest Satisfaction",
     ylab = "Distance from City Centre (km)",
     main = "Overall Guest Satisfaction vs Distance from City Centre (km)",
     col = rgb(0.85, 0.53, 0.10, alpha = 0.2), pch = 16)
#Most listings are typically less than 10 km away from the city centre, and they generally have high satisfaction scores. 
# Although there are outliers suggesting a possible positive correlation between greater distance from the city centre and higher guest satisfaction, they are too few to draw a definitive conclusion. 
# For listings with low scores despite being within 10 km of the city centre, other factors may be influencing their ratings.


