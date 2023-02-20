
library(ggplot2)
library(dplyr)
library(lubridate)

# Loading in datasets
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Viewing ny df
head(ny)

# Getting rows/columns of ny df
dim(ny)

# Viewing column names 
names(ny)

# Viewing washington df
head(wash)

# Getting rows/columns of wash df
dim(wash)

# Viewing column names 
names(wash)

# Viewing Chicago Dataset
head(chi)

# Getting numbers rows/columns of chicago dataset
dim(chi)

# Viewing column names in chi dataset
names(chi)

# Preparing datasets to concatenate
# Creating 'Gender' & 'Birth.Year' columns in wash df and filling with NA
wash$Gender = 'NA'
wash$Birth.Year = 'NA'

# Creating 'City' columns and filling with city values
wash$City = 'Washington'
ny$City = 'New York City'
chi$City = 'Chicago'

# Concatenate 3 datasets
df <- rbind(ny, wash, chi)
head(df)

# Checking dimensions of new concatenated df
dim(df)

# Viewing datatypes of df columns
str(df)

# Trip duration in seconds - create function by city
City.Trip.Duration = function(City){
    if (City == "New York City")
        return(mean(df[df$City == "New York City", 'Trip.Duration'], na.rm = TRUE))
    else if (City == "Washington")
        return(mean(df[df$City == "Washington", 'Trip.Duration'], na.rm = TRUE))
    else
        return(mean(df[df$City == "Chicago", 'Trip.Duration'], na.rm = TRUE))
}  


# Getting average trip duration in seconds by city

# NYC Trip Duration Mean calling function
NY.Duration.Mean = City.Trip.Duration('New York City')
# Chicago Trip Duration Mean calling function
Chi.Duration.Mean = City.Trip.Duration('Chicago')
# Wash Trip Duration Mean calling function
Wash.Duration.Mean = City.Trip.Duration('Washington')

print(round(Chi.Duration.Mean))
print(round(NY.Duration.Mean))
print(round(Wash.Duration.Mean))

# Bar graph of average trip duration by city
ggplot(aes(x = City, y = Trip.Duration), data = df) +
    geom_bar(position = 'stack', stat = "summary", fun.y = "mean", fill = "blue", color="black") + 
    ggtitle('The Average Travel Time for Users by City') +
    labs(y = 'Average Trip Duration', x = 'City')

# Histogram of Trip Duration by City
ggplot(aes(x=Trip.Duration), data=df) +
      geom_histogram(binwidth = 25, color="orange") +
      scale_x_continuous(limits = c(50, 5000), breaks = seq(0, 5000, 1000))+ 
      facet_wrap(~City) +
      ggtitle('Bike Share Trip Durations By City') +
      labs(y = 'Trip Duration', x = 'City')
      

# Summary of Trip Durations by City
by(df$Trip.Duration, df$City, summary)

# Number of User Types by City
by(df$User.Type, df$City, summary)

# Bar plot depicting number of Subscribers and Users by City
ggplot(df, aes(x = User.Type)) +
    geom_bar(stat = 'count', fill = "blue", color="black") + 
    facet_wrap(~City) +
    ggtitle('Bike Share Users By City') +
      labs(y = 'User Type', x = 'Number of Users')

# Scatterplot depicting Trip Durations by User Type
ggplot(df, aes(x = User.Type, y = Trip.Duration)) +
    geom_jitter(alpha = 0.5) +
    facet_wrap(~City) +
    ggtitle('Bike Share Trip Durations By User Type') +
      labs(x = 'User Type', y = 'Trip Duration')
  


# Summary of Bike Share Trip Durations by User Type
by(df$Trip.Duration, list(df$User.Type), summary)

# Converting str into datetime data type 
# Source for datetime in r: https://r4ds.had.co.nz/dates-and-times.html

dt_start <- ymd_hms(df$Start.Time)
df$Weekday <- wday(dt_start, label=TRUE, abbr=FALSE)


# Summary of User Types per Days of the Week
by(df$Weekday, df$User.Type, summary)

# Plotting User Types by Days of the Week
ggplot(df, aes(x=Weekday, fill=User.Type)) +
    geom_bar(position = 'dodge', stat='count') +
    scale_x_discrete(breaks = c(1,2,3,4,5,6,7), labels = c('Sun', 'Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat')) +
    labs(y = 'Number of Users', x = 'Days of Week') +
    ggtitle('Bike Share User Types by Day of Week')

system('python -m nbconvert Explore_bikeshare_data.ipynb')
