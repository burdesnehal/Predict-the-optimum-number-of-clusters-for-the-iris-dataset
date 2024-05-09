# Load or create your dataset
data <- read.csv("C:\\Users\\sneha\\OneDrive\\Desktop\\Iris.csv")  # Read the data from a CSV file
head(data)  # Print the first few rows of the data

# Assuming the first 4 columns are numeric features
iris_scaled <- scale(data[, 2:5])  # Scale the numeric features
head(iris_scaled)  # Print the first few rows of the scaled data

# Compute WCSS for different values of k
set.seed(123)  # Set a seed for reproducibility
wcss <- vector()  # Initialize a vector to store WCSS values

for (i in 1:10) {
  kmeans_model <- kmeans(iris_scaled, centers = i)  # Run k-means clustering with k = i
  wcss[i] <- kmeans_model$tot.withinss  # Store the WCSS value for k = i
}

# Plot the elbow method
elbow_plot <- ggplot(data.frame(k = 1:10, wcss = wcss), aes(x = k, y = wcss)) +
  geom_line(color = "#3366FF", linewidth = 1.2) +  # Add a line with custom color and thickness
  geom_point(color = "#3366FF", size = 3) +  # Add points with custom color and size
  labs(title = "The Elbow Method",
       x = "Number of Clusters",
       y = "Within Cluster Sum of Squares") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +  # Set x-axis tick marks
  scale_y_continuous(labels = comma) +  # Add thousands separators to y-axis
  theme_economist() +  # Apply a professional theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and format title
        axis.title = element_text(face = "bold", size = 12),  # Format axis titles
        axis.text = element_text(size = 10),  # Format axis text
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2),  # Add major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "white", color = "gray90"))  # Set panel background

print(elbow_plot)  # Print the elbow plot

# Determine optimal k (e.g., k = 3 based on the elbow)
optimal_k <- 3  # Assuming the optimal k is 3 based on the elbow plot

# Run k-means clustering with optimal k
kmeans_model <- kmeans(iris_scaled, centers = optimal_k)  # Run k-means clustering with k = 3

# Add cluster assignments to the data
data$cluster <- as.factor(kmeans_model$cluster)  # Add cluster assignments as a new column

# Visualize the clusters
ggplot(data, aes(x = SepalLengthCm, y = SepalWidthCm, color = cluster)) +
  geom_point(size = 3) +  # Add data points with larger size
  scale_color_discrete(name = "Cluster") +  # Add a legend for clusters
  ggtitle("K-means Clustering of Iris Dataset") +  # Add a title
  theme_minimal() +  # Apply a clean theme
  theme(plot.background = element_rect(fill = "lightgray"),  # Set background color
        panel.background = element_rect(fill = "white"),  # Set panel background color
        plot.title = element_text(hjust = 0.5))  # Center the title
