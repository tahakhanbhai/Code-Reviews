#Maaryah Salyani
#Assignment 4

getwd()
setwd("/Users/maaryahsalyani/ufo_assignment")
# Load the dplyr package

#loaded and intsalled dyplyr package
library(dplyr)
install.packages("dplyr")
library(stringr)

ufo.df <- read.csv("ufo_subset.csv")

#removed spaces from column names
nospace.colnames <- gsub(" ", "", colnames(ufo.df))
print(nospace.colnames)

#Great use of the gsub() function. The spaces from column names could have also been removed in the read.csv() function by using trimws(). The code would look like: read.csv(trimws('ufo_subset.csv')

#found rows with missing Shape information and impute with "unknown"
#use ifelse statement to replace missing information with the word "unknown"

missing_shape <- ifelse(is.na(ufo.df$Shape), "unknown", ufo.df$Shape)
print(missing_shape)

#The choice to use the ifelse function was great, as the logic in the statement makes sense. 
#However, many of the empty cells in the shape function do not contain NA values, but instead are just empty strings. So, the is.na() function does not work to find the missing cells and impute with unknown. 
#This could have been fixed using the na.strings() function when loading the dataset (in the read.csv function), or by using grepl() in this line to identify cells with an empty string pattern. 

# Removed the rows without Country information
no_country <- ufo.df[!is.na(ufo.df$country), ]
print(no_country)
#Same instance as the missing_shape call. Empty cells are not classified as NA so no rows are removed in the no_country dataframe. 

date <- ufo.df$datetime
print(date)
# Converted Datetime column to POSIXct format using the as.POSIXct() function
# as.POSIXct converts a data with time values into calendar date/time format
ufo.df$datetime <- as.POSIXct(ufo.df$datetime, format = "%Y-%m-%d %H:%M")
print(ufo.df$datetime)

# Convert Date_posted column to Date format using as.date function
# as.date converts data into date format when there is no time component
ufo.df$date_posted <- as.Date(ufo.df$date_posted, format = "%d-%m-%Y")
print(ufo.df$date_posted)


# Defined a pattern for identifying possible hoax reports using the term "HOAX"
# HOAX was chosen as an identifier as it appeared in several comments in the data set
hoax_filter <- "HOAX"

# Created a column called "hoax" using grepl function
# grepl will return the boolean value TRUE if found in the comments column
ufo.df$hoax <- grepl(hoax_filter, ufo.df$comments, ignore.case = TRUE)
#Great job using the grepl() function to create the new hoax column. Code works very effectively. 
#One extra aspect that could have been added were other key words in the dataset that likely indicated a hoax comment as well. 
#These kind of words include fake, or false. The strings inputted into grepl() are also case-sensitive, so the words hoax and Hoax could also have been added in. 


# Created a table that reports the number of hoax sightings per country
hoax_counts <- table(ufo.df$country[ufo.df$hoax])

# Created a table reporting the count of the total sightings per country
total_counts <- table(ufo.df$country)

# Calculated the percentage of hoax sightings per country
hoax_percentage <- (hoax_counts / total_counts) * 100

# Created a data frame with columns for the country, hoax number sightings, and hoax percentage
hoax_table <- data.frame(
  country = names(hoax_counts),
  hoax_count = hoax_counts,
  hoax_percentage = hoax_percentage
)

# Print hoax percent table
print(hoax_table)

#Hoax percentage was calculated properly, but the empty strings were not taken into account when the country column is blank. 
# So, in hoax_table, there is an empty row with a hoax frequency of 36 that is not attributed to any country. 
#The printed hoax_table also displays unnecessary information from the hoax_counts and hoax_percentage tables.
#The entire tables are printed out, so there are three columns in the table that display the country name.
#I'd recommend instead of creating three separate tables, try only creating one and adding the columns to that table accordingly using the dplyr function summarize. 
#The code could look like this: 
#hoax_per_country <- ufo.df %>%
#group_by(country) %>%
#  summarize(sightings = n(),
#            hoax_count = sum(is_hoax),
#            hoax_percentage = (hoax_count / sightings) * 100) 

# Converted the "datetime" and "date_posted" columns to Date format using as.Date function
ufo.df$datetime <- as.Date(ufo.df$datetime)
ufo.df$date_posted <- as.Date(ufo.df$date_posted, format = "%d-%m-%Y")
#The date_posted column was already in the Date format from beforehand. 

# Calculated the time difference in days between sighting date and report date
#used the as.integer function to convert any values into an integer so it can be substracted
ufo.df$report_delay <- as.integer(ufo.df$date_posted - ufo.df$datetime)

# Removed the rows where the report date was earlier than the sighting date
# used greater than or equal to the right-han dside value
ufo.df <- ufo.df[ufo.df$date_posted >= ufo.df$datetime, ]

# Print the updated dataset
print(ufo.df)

#tapply function will apply functions to both the ufo.df$report_delay data values and ufo.df$country
#FUN is the function being applied to each group, which is the mean
#removed any NA values
average_report_delay <- tapply(ufo.df$report_delay, ufo.df$country, FUN = mean, na.rm = TRUE)

# Created a data frame with country and average report_delay columns
average_report_delay_table <- data.frame( country = names(average_report_delay), average_report_delay = average_report_delay)

# Print the table
print(average_report_delay_table)
#Great job creating this table! Code was very precise and effective.

###ANALYSIS OF DURATION.SECONDS COLUMN#####
# Checked the data type of the "duration seconds" column
data_type <- class(df$duration.seconds) #Dataset was improperly called upon here, should have been ufo.df instead of df. 
# The data is numeric
print(data_type)

# Check for NA values in the duration seconds column
missing_values <- sum(is.na(ufo.df$duration.seconds))
print(missing_values)
# 1 NA value was found

# Check for missing values in the duration_seconds column
na_values <- is.na(df$duration_seconds) #ufo.df should be used instead of df
# Summed the number of missing values
sum_na_values <- sum(na_values)
#This code is the same as run above to check for the NA values, so it didn't really need to be run.  

# used an ifelse statement to indicate whether there are missing (NA) values in the duration.seconds columns
if (sum_na_values > 0) {
  print(paste("There are", sum_na_values, "have missing values in the duration.seconds column."))
} else {
  print("No missing values in the duration.seconds column.")
}
#there are no missing (NA) values

# Calculated the range of the duration_seconds column using "range" function
duration_range <- range(df$duration.seconds) #ufo.df
#When run with ufo.df, the range function returns two NA values. Another method to find the range is using summary(ufo.df$duration.seconds)

# Printed the range
print(paste("Range of duration.seconds:", duration_range))
#"Range of duration.seconds: 0.02"     "Range of duration.seconds: 82800000"

# Assuming your data frame is called 'ufo.df' and the column is named 'duration_seconds'.
# Checked for values greater than 100000
greater_than_100k <- ufo.df$duration.seconds > 100000

# checking where extreme values are found
# Subseted the data frame to retired  rows where duration_seconds is greater than 100000
subset_ufo.df <- ufo.df[greater_than_100k, ]
print(subset_ufo.df)
#Some of the sightings that were classified as "Hoax" appear to have very long duration seconds, notably 82800000 which belongs to a hoax claim
#Great observation! There could be a relationship between hoax sightings and unlikely duration.seconds values. 


# Assuming your data frame is called 'ufo.df' and the column is named 'duration_seconds'
# used dplry package to remove rows where 'duration_seconds' is equal to 82800000 since it may be a hoax and is an extreme value
library(dplyr)
ufo.df <- ufo.df %>% filter(duration.seconds != 82800000)

#laoded the ggplot2 package to create a histogra,
library(ggplot2)

# Converted column to numeric
ufo.df$duration_seconds <- as.numeric(ufo.df$duration_seconds) #This code doesn't work. It also is not really needed because the duration.seconds column already is numeric. Be careful of inputting incorrect column names. 
# used the geom_histogram function to divide the range of values into bins and counting the number of data points that fall into each bin
ggplot(data = ufo.df, aes(x = duration.seconds)) +
  geom_histogram(binwidth = 10000, col = "black", fill = "lightblue") +
  labs(title = "Duration Seconds Histogram", x = "Duration (seconds)", y = "Frequency") +
  scale_y_continuous(limits = c(0, 60)) +
  xlim(0, 7000000)  # changes the x-axis limits from 0 to 7000000
#Great job using the ggplot function to create the histogram. One thing I noticed about the histogram and changed in my own project is the heavy right skew.
#Its not needed but to make the histogram more readable, you could have log-distributed the values to make it resemble a more standardized curve. 
