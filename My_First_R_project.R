# install and load necessary packages
install.packages("dplyr")
install.packages("ggplot2")
# load the packages
library(dplyr)
library(ggplot2)

# Load the mtcars dataset
data("mtcars")
# Display the first few rows of the dataset
head(mtcars)

# Calculate the average (mean) miles per gallon (mpg) for each cylinder type (cyl)
average_mpg_by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarize(average_mpg = mean(mpg))

# Create a bar chart to visualize the average mpg by cylinder type
ggplot(average_mpg_by_cyl, aes(x = cyl, y = average_mpg)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average MPG by Cylinder Type",
    x = "Number of Cylinders",
    y = "Average MPG"
  )
