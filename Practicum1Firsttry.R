# Part 1

#This just loading all packages used in hw 
library(dplyr)
library(tibble)
library(lubridate)
library(readr)
library(ggplot2)

#Make the dataframe literally as mentioned in instructions
doctor_data <- data.frame(
  doctor_type = c("PCP", "Psychiatrist", "Surgeon", "Anesthesia"),
  doctor_lastname = c("Smith", "Dame", "Jones", "Zayas"),
  location = c("MA", "ME", "NH", "VT"),
  AVG_Rating = c(7, 9, 8, 9)
)

# Print the thing as mentioend by instructions
print(doctor_data)


selected_1_2 <- doctor_data[1, 2]  # Take the 1 row and 2 column. I got Smith

selected_2_to_4 <- doctor_data[2:4, ]  #  Take the rows 2 through 4 for all columns

selected_last_column <- doctor_data[, 4]  # Pick all rows in the fourth column

# Get column names
colnames(mtcars)

# Create the scatterplot
ggplot(mtcars, aes(x = mpg, y = hp, color = as.factor(cyl))) +  # Convert cyl to factor for discrete color scale
  geom_point() +
  labs(title = "MPG vs Horsepower in mtcars Dataset",
       x = "Miles Per Gallon (MPG)",
       y = "Horsepower (HP)",
       color = "Number of Cylinders") +
  theme_minimal()

## Plot shows the relationship between HP(Horsepower) and MPG(Miles per Gallon)
## of variouse cars in dataset. What I observe is an inverse relationship: cars
## with higher horspower tend to have lower Miles Per Gallon. Yet again, I know
## very little about cars but I thought higher horspower means higher miles per
## gallon so interesting to see the opposite. Also thats why i picked the two
## variables.
## The graph also shows the data for cylinders The trend is that cars with more
## cylinders have lower mpg

# Summary Statistics of dataset
summary(mtcars)

# TAKE Pearson Correlation Coefficient between 'wt' and 'mpg'
correlation_coefficient <- cor(mtcars$wt, mtcars$mpg, method = "pearson")

# Print coeffiecnt
print(correlation_coefficient)

# The Pearson corrleation coefficent for 'wt' and 'mpg' was calculated to be -0.867
# This statistic is a stat measure of linear relationship between two variables
#A value of -0.867 indicates a strong negative linear relationship between the weight of the cars and their fuel efficiency. 
#As the weight of the car increases, the fuel efficiency (measured in miles per gallon) tends to decrease. 
#This result is , as heavier cars generally need more energy to move, which causes lower fuel efficiency.


# Part 2

#Question 1
data <- read.csv("Substance_Use_Disorder_Treatment_Program_Admissions__Beginning_2007_20240212.csv")
data

# Question 2
dim(data)
# 99,367 rows and 11 columns
glimpse(data)
# This jsut a glimpse of data
summary(data)
# This shows various statsistcs available
# Interesting to see Admissions
# Min of 1, 1st quartile 2, median 8, Mean 41.91, 3rd quartile 28, Max 2861
# Will graph to see skewsness, i think this right skewed
# Thr first entries
head(data)
# The last entries
tail(data)

# NA
missing_values <- sum(is.na(data))
missing_values

# 0 missing values

frequency_table <- table(data$`Primary.Substance.Group`)
frequency_table

# Filter out the rows where 'Primary.Substance.Group' is 'None'
data <- subset(data, Primary.Substance.Group != "None")

# Basic statistics for the 'Admissions' column
admissions_desc <- summary(data$Admissions)
admissions_desc
# This just stats like previously talked about

# Plotting histogram using base R
hist(data$Admissions, 
     main = "Histogram of Admissions", 
     xlab = "Admissions", 
     col = "skyblue", 
     border = "white")

# Skewed to the right but graph is not the best

# Compute the IQR
IQR <- IQR(data$Admissions)
# Compute the upper bound to exclude outliers becasue lower bound is - its impossible
upper_bound <- quantile(data$Admissions, 0.75) + 1.5 * IQR
# Filter the data to exclude outliers
filtered_data <- subset(data, Admissions <= upper_bound)


# Histogram with logarithmic scale
ggplot(filtered_data, aes(x = Admissions)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_log10() +
  labs(title = "Histogram of Admissions (Log Scale)", x = "Admissions (log scale)", y = "Frequency") +
  theme_minimal()

# With the logarithmic scale, it's clear that while there are fewer instances of high admissions, 
# there's a more gradual decline in frequency than what the original histogram suggested.
#This distribution confirms that most treatment admissions are for smaller numbers of individuals, 
#with a steady but much smaller number of programs admitting larger groups.

# Detect outliers in the 'Admissions' column using the IQR method
Q1 <- quantile(data$Admissions, 0.25)
Q3 <- quantile(data$Admissions, 0.75)
IQR <- Q3 - Q1
lower_threshold <- Q1 - 1.5 * IQR
upper_threshold <- Q3 + 1.5 * IQR

Q1 
Q3 
IQR 
lower_threshold
upper_threshold

# Identify outliers and finding total outleirs in dataset
outliers <- data %>%
  filter(Admissions < lower_threshold | Admissions > upper_threshold)

num_outliers <- nrow(outliers)
num_outliers
# This is just getting outliers
print(paste("Number of missing values:", missing_values))
print(admissions_desc)
print(paste("Lower threshold for outliers:", lower_threshold)) ### Ignore becuse - impossible
print(paste("Upper threshold for outliers:", upper_threshold))
print(paste("Number of outliers:", num_outliers))

# Remove outliers
data_without_outliers <- subset(data, Admissions <= upper_threshold)
data_without_outliers

dim(data_without_outliers)
# Rows: 86,456
# Columns: 7
# The following is just some basic data exploration steps
glimpse(data_without_outliers)
summary(data_without_outliers)
head(data_without_outliers)
tail(data_without_outliers)

missing_vals<-sum(is.na(data_without_outliers))
missing_vals
# No missing values

frequency_table <- table(data_without_outliers$`Primary.Substance.Group`)
frequency_table

# Question 3

# Check the column names of the dataframe
colnames(data_without_outliers)

# Define the vectors for county codes and names
county_codes <- c("AL", "AG", "BX", "BM", "CA", "CY", "CH", "CM", "CN", "CL", "CO", "CR", "DE", "DU", "ER", "ES", "FR", "FU", "GE", "GR", "HE", "JE", "KG", "LE", "LI", "MA", "MO", "MG", "NA", "NY", "NI", "ON", "OD", "OT", "OR", "OL", "OS", "OG", "PU", "QN", "RE", "RD", "RO", "SL", "SA", "SC", "SH", "SY", "SE", "ST", "SU", "SV", "TI", "TO", "UL", "WR", "WS", "WA", "WE", "WY", "YA")
county_names <- c("Albany", "Allegany", "Bronx", "Broome", "Cattaraugus", "Cayuga", "Chautauqua", "Chemung", "Chenango", "Clinton", "Columbia", "Cortland", "Delaware", "Dutchess", "Erie", "Essex", "Franklin", "Fulton", "Genesee", "Greene", "Herkimer", "Jefferson", "Kings", "Lewis", "Livingston", "Madison", "Monroe", "Montgomery", "Nassau", "New_York_City", "Niagara", "Oneida", "Onondaga", "Ontario", "Orange", "Orleans", "Oswego", "Otsego", "Putnam", "Queens", "Rensselaer", "Richmond", "Rockland", "St_Lawrence", "Saratoga", "Schenectady", "Schoharie", "Schuyler", "Seneca", "Steuben", "Suffolk", "Sullivan", "Tioga", "Tompkins", "Ulster", "Warren", "Washington", "Wayne", "Westchester", "Wyoming", "Yates")

# Tibble
county_tibble <- tibble(county_code = county_codes, county_name = county_names)

# Print the tibble to check
print(county_tibble)

missing_vals<-sum(is.na(county_tibble))
missing_vals

# Create program codes
program_codes <- c("Crisis" = "CR", "Inpatient" = "IN", "Opioid Treatment Program" = "OT", 
                   "Outpatient" = "OP", "Residential" = "RE", "Specialized" = "SP")

# Assign the program codes to the Program.Category
program_category_df <- data_without_outliers %>%
  distinct(Program.Category) %>%
  mutate(program_code = program_codes[Program.Category])

# Check
print(program_category_df)


# Assign unique program codes
program_category_df <- data_without_outliers %>%
  distinct(Program.Category) %>%
  mutate(program_code = as.factor(Program.Category)) %>%
  mutate(program_code = as.numeric(program_code)) # Convert factor to numeric for a unique code

program_category_df

# Create substance codes
substance_codes <- c("Heroin" = "H", "All Others" = "AO", "Other Opioids" = "OO", 
                     "Alcohol" = "A", "Cocaine" = "C", "Marijuana" = "M")

# Assign the substance codes to the Primary.Substance.Group 
primary_substance_group_df <- data_without_outliers %>%
  distinct(Primary.Substance.Group) %>%
  mutate(substance_code = substance_codes[Primary.Substance.Group])

# Check
print(primary_substance_group_df)

admissions_data <- data_without_outliers %>%
  left_join(county_tibble, by = c("County.of.Program.Location" = "county_name")) %>%
  # Join with program_category_df to replace 'Program.Category' with 'program_code'
  left_join(program_category_df, by = c("Program.Category" = "Program.Category")) %>%
  # Join with primary_substance_group_df to replace 'Primary.Substance.Group' with 'substance_code'
  left_join(primary_substance_group_df, by = c("Primary.Substance.Group" = "Primary.Substance.Group")) %>%
  select(Year, 
         County.of.Program.Location = county_code,  # county_tibble
         Program.Category = program_code,           # program_category_df
         Service.Type, 
         Age.Group, 
         Primary.Substance.Group = substance_code,  # primary_substance_group_df
         Admissions)

# first few
print(head(admissions_data))

#Question 4

annualAdmissions <- function(data) {
  # Summarize the total admissions by year
  admissions_by_year <- data %>%
    group_by(Year) %>%
    summarise(Total_Adm = sum(Admissions, na.rm = TRUE)) %>%
    arrange(desc(Total_Adm))
  
  # Find the year with the highest number of admissions
  top_year <- admissions_by_year %>%
    filter(Total_Adm == max(Total_Adm))
  
  # Plot
  p <- ggplot(admissions_by_year, aes(x = Year, y = Total_Adm)) +
    geom_line() +
    geom_point() +
    geom_text(data = top_year, aes(label = Total_Adm, vjust = -1)) +
    scale_x_continuous(breaks = unique(admissions_by_year$Year)) + # Set x breaks to every year
    labs(title = "Total Number of Admissions per Year in NY",
         x = "Year",
         y = "Number of Admissions") +
    theme_minimal()
  
  print(p)
  
  # Explaination
  cat("The chart displays the total number of admissions per year for the entire state of New York.\n",
      "Each year is displayed on the x-axis. The year with the highest number of admissions is annotated on the chart.\n",
      "It provides a visual representation of trends over time in the volume of admissions.\n")
}

# Use the function 
annualAdmissions(admissions_data)

# Question 5

# Calculate the total admissions for each county
total_admissions_by_county <- data_without_outliers %>%
  group_by(County.of.Program.Location) %>%
  summarise(Total_Adm = sum(Admissions, na.rm = TRUE)) %>%
  ungroup()

# Use the total number of admissions to find percentages
grand_total_admissions <- sum(total_admissions_by_county$Total_Adm)

# Use the percentage of total admissions for each county
total_admissions_by_county <- total_admissions_by_county %>%
  mutate(Percentage = (Total_Adm / grand_total_admissions) * 100)

# Get the top 5 counties by percentage
top_5_counties <- total_admissions_by_county %>%
  arrange(desc(Percentage)) %>%
  slice_head(n = 5)

# Plotting the bar chart
ggplot(top_5_counties, aes(x = reorder(County.of.Program.Location, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  geom_text(aes(label = paste(round(Percentage, 2), "%")), vjust = -0.5) +
  labs(title = "Top 5 Counties by Percentage of Admissions",
       x = "County",
       y = "Percentage of Total Admissions") +
  theme_minimal()

## Erie, Onondaga, Queens, Suffolk and New York are presesnted in the graph

# Question 6

# Creating dataframe called Rehab_data which filters all facilities that include the word rehab, rehabilitation
Rehab_data <- data_without_outliers %>%
     filter(grepl("rehab|rehabilitation", Service.Type, ignore.case = TRUE))

# Identify the most prominent substance among each age group
most_prominent_substance <- Rehab_data %>%
     group_by(Age.Group) %>%
     summarise(Most_Prominent_Substance = names(which.max(table(Primary.Substance.Group))))

# Plot
ggplot(most_prominent_substance, aes(x = Age.Group, y = Most_Prominent_Substance, fill = Age.Group)) +
     geom_col() +
     labs(title = "Most Prominent Substance in Each Age Group for 'Rehab' Facilities",
          x = "Age Group",
          y = "Most Prominent Substance") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better readability

## Look at above (# 18 THROUGH 24, 25 THROUGH 34 USE HEROIN, UNDER 18 IS Marijuana and the rest is alcohol)


# Filter admissions to "Rehab" facilities
rehab_admissions <- admissions_data_df %>%
  filter(grepl("rehab", Service.Type, ignore.case = TRUE))

# Group by age group and primary substance group, then summarize to find the most prominent substance
prominent_substance_by_age <- rehab_admissions %>%
  group_by(Age.Group, primary_substance_group) %>%
  summarise(total_admissions = sum(Admissions)) %>%
  arrange(Age.Group, desc(total_admissions)) %>%
  slice(1) # Selects the top substance for each age group

# Visualize the results using a bar chart
ggplot(prominent_substance_by_age, aes(x = Age.Group, y = total_admissions, fill = primary_substance_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Prominent Substance by Age Group in Rehab Facilities",
       x = "Age Group",
       y = "Total Admissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for readability

