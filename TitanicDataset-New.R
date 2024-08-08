install.packages("dplyr")
library(dplyr)

data <- read.csv ("Titanic-Dataset.csv")
head(data)
summary(data)
View(data)
str(data)
colSums(is.na(data))


data <- data %>% mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age))

data <- data %>%
  mutate(Survived = as.factor(Survived))

data <- data %>% select(-Cabin)

data <- data %>% mutate(Survived = as.factor(Survived), Pclass = as.factor(Pclass), Sex = as.factor(Sex), Embarked = as.factor(Embarked)) 

names(data)
num_blank_embarked <- sum(data$Embarked == "")

write.csv(data, "Cleaned_Titanic_Data.csv", row.names = FALSE)
install.packages("plotly")
library(plotly)

titanic_data <-data

names(titanic_data) <- tolower(names(titanic_data))


# Create a scatter plot of Age vs. Fare colored by Embarkation
ggplot_scatter <- ggplot(titanic_data, aes(x = age, y = fare, color = embarked)) +
  geom_point() +
  xlab("Age") +
  ylab("Fare") +
  ggtitle("Scatter Plot of Age vs. Fare by Embarkation") +
  theme_minimal()


# Display the plot
ggplot_scatter

# Calculate counts and percentages for interactive plot
titanic_summary <- titanic_data %>%
  group_by(pclass, survived) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100, 
         survival_status = ifelse(survived == 1, "Survived", "Did Not Survive"))

# Convert to data frame
titanic_summary <- as.data.frame(titanic_summary)

# Create the interactive stacked bar plot
plot <- plot_ly(titanic_summary, 
                x = ~pclass, 
                y = ~percentage, 
                type = 'bar', 
                color = ~as.factor(survived),
                text = ~paste('Status:', survival_status, '<br>Percentage:', round(percentage, 2), '%'),
                hoverinfo = 'text',
                textposition = 'auto') %>%
  layout(barmode = 'stack',
         xaxis = list(title = 'Passenger Class'),
         yaxis = list(title = 'Percentage'),
         title = 'Survival Proportions by Passenger Class',
         legend = list(title = list(text = 'Survival Status')))

plot
