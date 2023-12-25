library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)

## Vulturedata

tagdata = readRDS("tagdata.rds")

# Check the number of different bird IDs
num_unique_ids <- length(unique(tagdata$LocalID))

# Print the result
cat("Number of different bird IDs:", num_unique_ids)

dat <- tagdata #renaming the dataframe as "dat"

#Basic data exploration

dim(dat)
names(dat)
str(dat)
summary(dat)

######################################################################
# set Date/Time
# convert data/time format

dat$Datetime <- as.POSIXct(dat$datetime, format="%Y-%m-%d %H:%M", tz = "UTC") 

# check
summary(dat$Datetime)
range(dat$Datetime)

# useful additional time columns

dat$Date <- as.Date(dat$datetime)  
dat$week <- as.numeric(strftime(as.POSIXlt(dat$datetime),format="%W"))
dat$month <- as.numeric(strftime(as.POSIXlt(dat$datetime),format="%m"))
dat$year <- as.numeric(strftime(as.POSIXlt(dat$datetime),format="%Y"))
dat$day <- as.numeric(strftime(as.POSIXlt(dat$datetime),format="%j"))

# check the additional time columns

summary(dat)

######################################################################
# inspect data
# N individuals; N data per individual; sampling period per individual; 
# sampling regime per individual
# --> how many individuals? how much data for each? sampled when? sampled how frequently?  

# N individuals
# Convert LocalID column to factor and define levels
dat$LocalID <- as.factor(dat$LocalID)
levels(dat$LocalID) <- unique(dat$LocalID)

# Verify the levels
levels(dat$LocalID)


length(levels(dat$LocalID))

# N locations per individuals
# table()
sort(table(dat$LocalID))


# Useful histogram graphs
hist(table(dat$LocalID), xlab = "N locations", 
     main = "Distribution of individual sample size \n N = 79 individuals")

# adding a numeric individual ID variable and plot a sort of 'Ghant chart' for each individual
# note the use of 'foo' and 'foo2' as names for temporary objects, which can then be 
# removed once used  

foo <- data.frame(LocalID = levels(dat$LocalID), 
                  numID = 1:length(unique(dat$LocalID)))
# we could have written'1:79', but the above is of course more flexible 

# to see the object we just created
foo

# merge with the location data
foo2 <- merge(dat, foo, by = "LocalID", all.x = TRUE, all.y = FALSE)

# check
dim(dat); dim(foo2)


# thus all in exactly same order, hence easy to add column without using merge()

# add column (no need to use merge(), see above why)
dat$numID <- foo2$numID

# clean the workspace
rm(foo, foo2)

# here now our plot of sampling times per individual
library(lattice)
xyplot(LocalID ~ Datetime, data = dat, groups = LocalID, 
       auto.key=list(columns  =  8))


######################################################################  
# ORDER DATA (time)
# order a dataframe, by individual and time 

datSrt <- dat[order(dat$LocalID,dat$datetime),]


#######################################################################
# DUPLICATE LOCATIONS?

which(datSrt$datetime[1:(nrow(datSrt)-1)] == datSrt$datetime[2:(nrow(datSrt))])


######################################################################
# it can come handy to add an identifier variable, identifying the first location 
# of each individual - let us call this 'firstRec'

foo <- which(datSrt$LocalID[1:(nrow(datSrt)-1)] != 
               datSrt$LocalID[2:(nrow(datSrt))])

datSrt$firstRec <- rep(0,nrow(datSrt))

datSrt$firstRec[foo+1] <- 1	# think why 'foo + 1'

datSrt$firstRec[1] <- 1

# let us check if this is correct

length(unique(datSrt$LocalID)) # count N individuals
sum(datSrt$firstRec)	
datSrt[sort(c(foo-1,foo,foo+1)),c('LocalID','datetime','firstRec')] # first records seem correctly identified
rm(foo) 	# keeping workspace clean

######################################################################
######################################################################
# step lengths - Euclidean distance formula, as we area dealing with projected coordinates

foo <- sqrt((datSrt$x[1:(nrow(datSrt)-1)] - datSrt$x[2:(nrow(datSrt))])^2 + 
              (datSrt$y[1:(nrow(datSrt)-1)] - datSrt$y[2:(nrow(datSrt))])^2)

# Summary
summary(foo)	
datSrt$SL <- c(NA, foo) 
rm(foo)


##########################################################################
##########################################################################
# time between steps

foo <- difftime(datSrt$datetime[2:nrow(datSrt)], datSrt$datetime[1:(nrow(datSrt)-1)], units = "hour")
foo <- c(NA, foo)
summary(as.numeric(foo))
foo <- ifelse(datSrt$firstRec == 1, NA, foo)
summary(as.numeric(foo))
datSrt$dthr <- foo     # 'dt' stands for 'difference in time'
rm(foo)  

##########################################################################
# investigate distribution of step length and time lags between steps

datSrt$dt <- ifelse(is.na(datSrt$dthr), 0, as.numeric(datSrt$dthr))

hist(datSrt$dt); summary(datSrt$dt)		
datSrt$dt <- as.numeric(datSrt$dt)

## Speed ####

datSrt <- transform(datSrt, kmph = SL / dt)


# Calculate the time duration and count of locations for each bird ID

data_duration <- dat %>%
  group_by(LocalID) %>%
  summarize(Start = min(Datetime),
            End = max(Datetime),
            Locations = n()) %>%
  mutate(Duration_days = as.numeric(difftime(End, Start, units = "days")),  # Calculate duration in days
         Duration_hours = as.numeric(difftime(End, Start, units = "hours"))) %>%
  arrange(Duration_days)  # Arrange dataframe by number of days tracked in ascending order


# Create a grouped bar plot
ggplot(data_duration, aes(x = LocalID, y = Duration_days, fill = LocalID)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", Duration_days), y = Duration_days), vjust = -0.8, size = 2.5, color = "black", fontface = "bold") +
  scale_fill_hue(l = 50) +
  scale_y_continuous(name = "Duration (days)", expand = c(0, 0)) +
  scale_x_discrete(name = "Bird ID", limits = rev(ordered_ids)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

unique(data_duration$LocalID)


# Create a vector of unique bird IDs with more than 300 days of data
bird_ids_more_than_300_days <- data_duration$LocalID[data_duration$Duration_days > 300]

# Filter rows in datSrt dataframe based on the bird IDs with more than 300 days of data
datSrt38 <- datSrt[datSrt$LocalID %in% bird_ids_more_than_300_days, ]

# Get the number of unique LocalIDs
num_unique_LocalIDs <- length(unique(datSrt38$LocalID)) #38 birds

# Get the names of each unique LocalID
unique_LocalID_names <- unique(datSrt38$LocalID)

## Trying to run a loop to derive DMP for each bird (n=38)

install.packages("purrr")
library(purrr)

# Initialize an empty list to store the results for each bird ID
result_list <- list()

# Get all unique bird IDs from the dat dataframe
unique_bird_ids <- unique(datSrt38$LocalID)

# Define a function to calculate DOD, DMD, DDT, and SDP for each bird ID
calculate_daily_parameters <- function(bird_id) {
  # Subset the data for the current bird ID
  bird_data <- filter(datSrt38, LocalID == bird_id)
  
  # Calculate DDT for each day
  DDT <- bird_data %>%
    group_by(Date) %>%
    summarize(DDT = sum(SL, na.rm = TRUE))
  
  # Calculate displacement between first and each consecutive location for each day and bird
  bird_data <- bird_data %>%
    group_by(Date) %>%
    mutate(Dwrtf = sqrt((x - first(x))^2 + (y - first(y))^2))
  
  # Calculate maximum displacement wrt first location for each day and bird
  DMD <- bird_data %>%
    group_by(Date) %>%
    summarize(DMD = max(Dwrtf))
  
  # Calculate DOD for each day
  DOD <- bird_data %>% 
    group_by(Date) %>%
    summarize(DOD = sqrt((last(x) - first(x))^2 + (last(y) - first(y))^2))
  
  # Calculate SDP for each day
  merged_df <- left_join(DDT, DMD, by = "Date") %>%
    left_join(., DOD, by = "Date") %>%
    mutate(SDP = DMD / DDT) %>%
    select(Date, DOD, DMD, DDT, SDP)
  
  # Add the bird ID column to the dataframe
  merged_df$LocalID <- bird_id
  
  return(merged_df)
}

# Use map to iterate over each unique bird ID and calculate the parameters
result_list <- map(unique_bird_ids, calculate_daily_parameters)

# Combine all the individual dataframes into a single dataframe
DMP1 <- do.call(rbind, result_list)


# Extract month from the Date column and store it in a new column called 'Month'
DMP1$Month <- month(DMP1$Date)
DMP1$my <- format(DMP1$Date, "%b-%y")

unique(DMP1$LocalID)

### Checking unique values of months 
unique_values <- unique(DMP1$Month)
length(unique_values)
print(unique_values)

DMP1 %>%
  group_by(LocalID) %>%
  summarise(unique_months = list(unique(Month)), num_unique_months = length(unique(Month))) %>%
  arrange(num_unique_months)

# Birds with complete data (12 Months)
complete_birds <- DMP1 %>%
  group_by(LocalID) %>%
  filter(length(unique(Month)) == 12) %>%
  pull(LocalID) %>% unique()

# Filter data to only include complete birds
DMP1_complete <- DMP1 %>% filter(LocalID %in% complete_birds)
DMP1 <- DMP1_complete
unique(DMP1$LocalID)

###########VISUALIZATION OF BIRD TRAJECTORIES/MAPS################

library(ggplot2)
library(dplyr)

# Filter rows in datSrt where LocalID matches with LocalID in DMP1
datSrt_filtered <- datSrt %>%
  filter(LocalID %in% DMP1$LocalID)

remove_extreme_values <- function(data, variable, lower_quantile = 0.01, upper_quantile = 0.99) {
  lower_bound <- quantile(data[[variable]], lower_quantile, na.rm = TRUE)
  upper_bound <- quantile(data[[variable]], upper_quantile, na.rm = TRUE)
  data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
  return(data)
}

datSrt37_clean <- datSrt_filtered %>%
  group_by(LocalID) %>%
  group_modify(~ remove_extreme_values(.x, "x")) %>%
  group_modify(~ remove_extreme_values(.x, "y")) %>%
  ungroup()

# Group by "LocalID" and calculate the difference between the maximum and minimum date
days_of_data_datSrt37 <- datSrt37_clean %>%
  group_by(LocalID) %>%
  summarise(start_date = min(Datetime), 
            end_date = max(Datetime), 
            days = as.numeric(difftime(end_date, start_date, units = "days")) + 1) %>%
  ungroup() %>%
  arrange(desc(days)) # Order by number of days, from highest to lowest

# Print the result
print(days_of_data_datSrt37)

# Summarize the number of data points for each LocalID in datSrt12
count_datSrt37 <- datSrt37_clean %>%
  group_by(LocalID) %>%
  summarise(count_before = n()) %>%
  ungroup()

# Summarize the number of data points for each LocalID in datSrt12_clean
count_datSrt37_clean <- datSrt37_clean %>%
  group_by(LocalID) %>%
  summarise(count_after = n()) %>%
  ungroup()


library(leaflet)
library(mapview)
library(leaflet.extras)

# Create a palette with a color for each unique bird ID
color_palette <- rainbow(length(unique(datSrt37_clean$LocalID)))

# Create a named list of colors with bird IDs as names
bird_colors <- setNames(color_palette, unique(datSrt37_clean$LocalID))

# Create a basic leaflet map
m <- leaflet() %>%
  addTiles() # This adds the default OpenStreetMap tiles

# Loop through unique bird IDs and add trajectories for each bird
for (bird_id in unique(datSrt37_clean$LocalID)) {
  bird_data <- datSrt37_clean[datSrt37_clean$LocalID == bird_id,]
  m <- addPolylines(m, data = bird_data, lng = ~lon, lat = ~lat, color = bird_colors[[as.character(bird_id)]], weight = 2)
}

# Manually set labels for the legend
legend_labels <- unique(datSrt37_clean$LocalID)

# Create a named list of colors for legend
legend_colors <- setNames(color_palette, legend_labels)

# Add a legend
m <- addLegend(m, "bottomright", 
               colors = legend_colors, 
               labels = legend_labels, 
               title = "Bird IDs")

# Add scale bar
m <- addScaleBar(m, options = list(position = "bottomleft"))

# Print the map
m

### FACETWRAP VISUALIZATION ### INDIVIDUAL BIRD TRAJECTORY GRIDED ##
days_tracked <- datSrt37_clean %>%
  group_by(LocalID) %>%
  summarise(days = n_distinct(Datetime)) %>%
  arrange(desc(days)) %>%
  mutate(label = paste(LocalID, "\nDays tracked:", days))

# Create a plot using the prepared data
trajplotfacet <- datSrt37_clean %>%
  left_join(days_tracked, by = "LocalID") %>%
  arrange(LocalID, Datetime) %>%
  group_by(LocalID) %>%
  mutate(point_type = case_when(
    row_number() == 1 ~ "Start",
    row_number() == n() ~ "End",
    TRUE ~ "Intermediate"
  )) %>%
  ungroup() %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = point_type, shape = point_type), size = 1, alpha = 0.6) +
  geom_point(data = . %>% filter(point_type != "Intermediate"), aes(color = point_type, shape = point_type), size = 3) +
  scale_shape_manual(values = c("Start" = 16, "End" = 17, "Intermediate" = 1)) +
  scale_color_manual(values = c("Start" = "green", "End" = "red", "Intermediate" = "black")) +
  facet_wrap(~ factor(LocalID, levels = days_tracked$LocalID, labels = days_tracked$label)) +
  labs(title = "UTM Scatter Plot of Each Bird",
       x = "Easting (UTM)",
       y = "Northing (UTM)") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 10, face = "bold")
  )

print(trajplotfacet)

### EDA for Daily Movement Parameters Data #####

library(mgcv)
library(dplyr)
library(ggplot2)
library(mgcViz)


## EDA for DOD vs Month ##########

DMP1 %>%
  group_by(Month, LocalID) %>%
  summarise(mean_DOD = mean(DOD, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Month), y = mean_DOD, group = LocalID)) +
  geom_point(color = "red") +
  geom_line(color = "blue", size = 1.5) +
  facet_wrap(~ LocalID, scales = "free_y", labeller = label_bquote(rows = .(LocalID))) +
  labs(title = "Mean DOD vs Month for Each Bird",
       x = "Month",
       y = "Mean DOD (Km)") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Added this line to tilt month labels
  )


#### DMD vs Month

DMP1 %>%
  group_by(Month, LocalID) %>%
  summarise(mean_DMD = mean(DMD, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Month), y = mean_DMD, group = LocalID)) +
  geom_point(color = "red") +
  geom_line(color = "blue", size = 1.5) +
  facet_wrap(~ LocalID, scales = "free_y", labeller = label_bquote(rows = .(LocalID))) +
  labs(title = "Mean DMD vs Month for Each Bird",
       x = "Month",
       y = "Mean DMD (Km)") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),axis.text.x = element_text(angle = 45, hjust = 1)
  )

### DDT mean vs Month
DMP1 %>%
  group_by(Month, LocalID) %>%
  summarise(mean_DDT = mean(DDT, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Month), y = mean_DDT, group = LocalID)) +
  geom_point(color = "red") +
  geom_line(color = "blue", size = 1.5) +
  facet_wrap(~ LocalID, scales = "free_y", labeller = label_bquote(rows = .(LocalID))) +
  labs(title = "Mean DDT vs Month for Each Bird",
       x = "Month",
       y = "Mean DDT (Km)") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),axis.text.x = element_text(angle = 45, hjust = 1)
  )

### SDP vs Month

DMP1 %>%
  group_by(Month, LocalID) %>%
  summarise(mean_SDP = mean(SDP, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Month), y = mean_SDP, group = LocalID)) +
  geom_point(color = "red") +
  geom_line(color = "blue", size = 1.5) +
  facet_wrap(~ LocalID, scales = "free_y", labeller = label_bquote(rows = .(LocalID))) +
  labs(title = "Mean SDP vs Month for Each Bird",
       x = "Month",
       y = "Mean SDP") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),axis.text.x = element_text(angle = 45, hjust = 1)
  )


library(mgcv)

### GAM #### 
### AIC values for each bird model with different splines were checked with different daily movement parameters i.e. DDT,DOD,DMD and SDP

# Empty dataframe to store AIC values
aic_values <- data.frame(LocalID = character(),
                         SplineType = character(),
                         AIC = numeric())

# List of spline types
splines <- c("cr", "cc", "tp")

# Loop over each spline type for each bird
for (bird in unique(DMP1$LocalID)) {
  bird_data <- DMP1[DMP1$LocalID == bird, ]
  
  # Loop over each spline type
  for (spline in splines) {
    # Fit the model
    model <- gam(SDP + 0.000005 ~ s(Month, bs = spline), 
                 data = bird_data, 
                 family = betar(link = "logit"), 
                 method = "REML") ### DDT, DOD, DMD were also modelled in similar fashion with fashion with "family = gamma and link = "log" ### 
    
    
    # Store AIC value
    aic_values <- rbind(aic_values, data.frame(LocalID = bird, SplineType = spline, AIC = AIC(model)))
  }
}

# Group by LocalID and flag the minimum AIC value for each bird
aic_values <- aic_values %>%
  group_by(LocalID) %>%
  mutate(is_min = ifelse(AIC == min(AIC), TRUE, FALSE)) %>%
  ungroup()

# Plotting AIC values 

# Visualization
# Convert AIC_label to numeric
aic_values$AIC_label <- round(aic_values$AIC, 1)
aic_values$AIC_label <- as.numeric(as.character(aic_values$AIC_label))

# Visualization
plot <- ggplot(aic_values, aes(x = SplineType, y = AIC_label, fill = is_min, label = AIC_label)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  geom_text(aes(y = AIC_label + 0.1), position = position_dodge(width = 0.7), vjust = -0.5, angle = 90) + 
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray"), name = "Lowest AIC") +
  labs(x = "Spline Type", y = "AIC Value", title = "AIC Values for Each Bird by Spline Type, SDP ~ Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ LocalID) # Facet wrap by Bird IDs

print(plot)

####### Cyclic cubic spline was used for each daily movement parameter to fit the data (Dec-Jan transition) for each daily movement parameter, for each bird ###

## FOR DMD#######################

# Define the spline types
spline_types <- c("cc")

# Fit best GAM models for each bird
fit_best_gam <- function(DMP1) {
  best_aic <- Inf
  best_model <- NULL
  
  for (spline_type in spline_types) {
    gam_model <- gam(DMD + 0.0000005 ~ s(Month, bs = spline_type), data = DMP1, family = Gamma(link = "log"), method = "REML")
    
    if (AIC(gam_model) < best_aic) {
      best_aic <- AIC(gam_model)
      best_model <- gam_model
    }
  }
  
  return(best_model)
}

bird_models_DMD <- DMP1 %>%
  group_by(LocalID) %>%
  do(model = fit_best_gam(.))

# Function to generate predictions and confidence intervals
get_predictions <- function(model, birdID) {
  new_data <- data.frame(Month = 1:12) # Months from 1 to 12
  predictions <- predict(model, newdata = new_data, se.fit = TRUE)
  ci_width <- qt(0.975, df = model$df.residual) * predictions$se.fit # 95% CI
  new_data$Predicted_DMD <- exp(predictions$fit) # Convert from log scale
  new_data$Lower_CI <- exp(predictions$fit - ci_width)
  new_data$Upper_CI <- exp(predictions$fit + ci_width)
  new_data$LocalID <- birdID
  return(new_data)
}

all_predictions <- bind_rows(mapply(get_predictions, bird_models_DMD$model, bird_models_DMD$LocalID, SIMPLIFY = FALSE))

# Plot predictions with confidence intervals and facets
plot <- ggplot() +
  # Confidence intervals (light blue ribbon)
  geom_ribbon(data = all_predictions, aes(x = Month, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "blue") +
  # Predicted fit (blue line in the foreground)
  geom_line(data = all_predictions, aes(x = Month, y = Predicted_DMD), color = "blue", size = 1.2) +
  labs(x = "Month", y = "DMD (Km)") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  facet_wrap(~ LocalID, scales = "free", ncol = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

print(plot)

# Identify the months with minima and maxima for DMD
min_max_dmd <- all_predictions %>%
  group_by(LocalID) %>%
  summarise(Min_Month = Month[which.min(Predicted_DMD)],
            Max_Month = Month[which.max(Predicted_DMD)])

# Count the occurrences for each month and type (minima and maxima)
min_max_counts <- min_max_dmd %>%
  pivot_longer(cols = c(Min_Month, Max_Month), names_to = "Type", values_to = "Month") %>%
  group_by(Month, Type) %>%
  summarise(Count = n()) %>%
  ungroup()

# Add zero counts for missing months
missing_months <- setdiff(1:12, unique(min_max_counts$Month))
missing_data <- expand.grid(Month = missing_months, Type = c("Min_Month", "Max_Month"), Count = 0)
min_max_counts <- bind_rows(min_max_counts, missing_data)

# Plot the data as side-by-side bars for DMD
plot <- ggplot(min_max_counts, aes(x = factor(Month), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + # Side-by-side bar plot
  
  # Add annotations here
  geom_text(aes(label = Count, group = Type), 
            position = position_dodge(width = 0.9), vjust = -0.5, 
            color = "black", size = 3.5) +
  
  scale_fill_manual(values = c("Min_Month" = "blue", "Max_Month" = "red")) +
  labs(x = "Month",
       y = "Count",
       title = "Occurrence of the Minima and Maxima Mean Monthly Daily Maximum Displacement (DMD) for Each Bird") +
  scale_x_discrete(breaks = 1:12,
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

print(plot) # Print the plot


### FOR DOD

# Define the spline types
spline_types <- c("cc")

# Fit best GAM models for each bird using DOD as response variable
fit_best_gam <- function(bird_data) {
  best_aic <- Inf
  best_model <- NULL
  
  for (spline_type in spline_types) {
    gam_model <- gam(DOD + 0.0000005 ~ s(Month, bs = spline_type), data = bird_data, family = Gamma(link = "log"), method= "REML")
    
    if (AIC(gam_model) < best_aic) {
      best_aic <- AIC(gam_model)
      best_model <- gam_model
    }
  }
  
  return(best_model)
}

bird_models_DOD <- DMP1 %>%
  group_by(LocalID) %>%
  do(model = fit_best_gam(.))

# Function to generate predictions and confidence intervals for DOD
get_predictions <- function(model, birdID) {
  new_data <- data.frame(Month = 1:12) # Months from 1 to 12
  predictions <- predict(model, newdata = new_data, se.fit = TRUE)
  ci_width <- qt(0.975, df = model$df.residual) * predictions$se.fit # 95% CI
  new_data$Predicted_DOD <- exp(predictions$fit) # Convert from log scale
  new_data$Lower_CI <- exp(predictions$fit - ci_width)
  new_data$Upper_CI <- exp(predictions$fit + ci_width)
  new_data$LocalID <- birdID
  return(new_data)
}

all_predictions <- bind_rows(mapply(get_predictions, bird_models_DOD$model, bird_models_DOD$LocalID, SIMPLIFY = FALSE))

# Plot predictions with confidence intervals and facets for DOD
plot <- ggplot() +
  # Confidence intervals (light blue ribbon)
  geom_ribbon(data = all_predictions, aes(x = Month, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "blue") +
  # Predicted fit (blue line in the foreground)
  geom_line(data = all_predictions, aes(x = Month, y = Predicted_DOD), color = "blue", size = 1.2) +
  labs(x = "Month", y = "DOD (Km)") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  facet_wrap(~ LocalID, scales = "free", ncol = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

print(plot)

# Identify the months with minima and maxima for DOD
min_max_dod <- all_predictions %>%
  group_by(LocalID) %>%
  summarise(Min_Month = Month[which.min(Predicted_DOD)],
            Max_Month = Month[which.max(Predicted_DOD)])

# Count the occurrences for each month and type (minima and maxima)
min_max_counts <- min_max_dod %>%
  pivot_longer(cols = c(Min_Month, Max_Month), names_to = "Type", values_to = "Month") %>%
  group_by(Month, Type) %>%
  summarise(Count = n()) %>%
  ungroup()

# Add zero counts for missing months
missing_months <- setdiff(1:12, unique(min_max_counts$Month))
missing_data <- expand.grid(Month = missing_months, Type = c("Min_Month", "Max_Month"), Count = 0)
min_max_counts <- bind_rows(min_max_counts, missing_data)

# Plot predictions with confidence intervals and facets for DOD
plot <- ggplot() +
  geom_ribbon(data = all_predictions, aes(x = Month, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "blue") +
  geom_line(data = all_predictions, aes(x = Month, y = Predicted_DOD), color = "blue", size = 1.2) +
  labs(x = "Month", y = "DOD (Km)") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  facet_wrap(~ LocalID, scales = "free", ncol = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

print(plot)

# Plot the data as side-by-side bars for DOD
# Plot the data as side-by-side bars for DOD with annotations
plot <- ggplot(min_max_counts, aes(x = factor(Month), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + # Side-by-side bar plot
  
  # Add annotations here
  geom_text(aes(label = Count, group = Type), 
            position = position_dodge(width = 0.9), vjust = -0.5, 
            color = "black", size = 3.5) +
  
  scale_fill_manual(values = c("Min_Month" = "blue", "Max_Month" = "red")) +
  labs(x = "Month",
       y = "Count",
       title = "Occurrence of the Minima and Maxima Mean Monthly Daily Overall displacement (DOD) for Each Bird") +
  scale_x_discrete(breaks = 1:12,
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

print(plot)


### FOR SDP

# Define the spline types
spline_types <- c("cc")

# Fit best GAM models for each bird using SDP as response variable
fit_best_gam <- function(bird_data) {
  best_aic <- Inf
  best_model <- NULL
  
  for (spline_type in spline_types) {
    gam_model <- gam(SDP + 0.0000005 ~ s(Month, bs = spline_type), data = bird_data, family = betar(link = "logit"), method = "REML")
    
    if (AIC(gam_model) < best_aic) {
      best_aic <- AIC(gam_model)
      best_model <- gam_model
    }
  }
  
  return(best_model)
}

bird_models_SDP <- DMP1 %>%
  group_by(LocalID) %>%
  do(model = fit_best_gam(.))

# Logistic function for transforming predictions
logistic <- function(x) {
  return(1 / (1 + exp(-x)))
}

# Function to generate predictions and confidence intervals for SDP
get_predictions <- function(model, birdID) {
  new_data <- data.frame(Month = 1:12) # Months from 1 to 12
  predictions <- predict(model, newdata = new_data, se.fit = TRUE)
  ci_width <- qt(0.975, df = model$df.residual) * predictions$se.fit # 95% CI
  
  # Transforming using the logistic function
  new_data$Predicted_SDP <- logistic(predictions$fit)
  new_data$Lower_CI <- logistic(predictions$fit - ci_width)
  new_data$Upper_CI <- logistic(predictions$fit + ci_width)
  new_data$LocalID <- birdID
  
  return(new_data)
}

all_predictions <- bind_rows(mapply(get_predictions, bird_models_SDP$model, bird_models_SDP$LocalID, SIMPLIFY = FALSE))

# Plot predictions with confidence intervals and facets for SDP
plot <- ggplot() +
  geom_ribbon(data = all_predictions, aes(x = Month, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "blue") +
  geom_line(data = all_predictions, aes(x = Month, y = Predicted_SDP), color = "blue", size = 1.2) +
  labs(x = "Month", y = "SDP") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  facet_wrap(~ LocalID, scales = "free", ncol = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

print(plot)

# Identify the months with minima and maxima for SDP
min_max_sdp <- all_predictions %>%
  group_by(LocalID) %>%
  summarise(Min_Month = Month[which.min(Predicted_SDP)],
            Max_Month = Month[which.max(Predicted_SDP)])

# Count the occurrences for each month and type (minima and maxima)
min_max_counts_sdp <- min_max_sdp %>%
  pivot_longer(cols = c(Min_Month, Max_Month), names_to = "Type", values_to = "Month") %>%
  group_by(Month, Type) %>%
  summarise(Count = n()) %>%
  ungroup()

# Add zero counts for missing months
missing_months_sdp <- setdiff(1:12, unique(min_max_counts_sdp$Month))
missing_data_sdp <- expand.grid(Month = missing_months_sdp, Type = c("Min_Month", "Max_Month"), Count = 0)
min_max_counts_sdp <- bind_rows(min_max_counts_sdp, missing_data_sdp)

# Ensure unique months for each Type
min_max_counts_sdp <- min_max_counts_sdp %>%
  group_by(Month, Type) %>%
  summarise(Count = sum(Count)) %>%
  ungroup()


# Plot the data as side-by-side bars for SDP with annotations
plot_sdp <- ggplot(min_max_counts_sdp, aes(x = factor(Month), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + # Side-by-side bar plot
  
  # Add annotations here
  geom_text(aes(label = Count, group = Type), 
            position = position_dodge(width = 0.9), vjust = -0.5, 
            color = "black", size = 3.5) +
  
  scale_fill_manual(values = c("Min_Month" = "blue", "Max_Month" = "red")) +
  labs(x = "Month",
       y = "Count",
       title = "Occurrence of the Minima and Maxima Mean Monthly SDP for Each Bird") +
  scale_x_discrete(breaks = 1:12,
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

print(plot_sdp)


### FOR DDT

# Define the spline types
spline_types <- c("cc")

# Fit best GAM models for each bird using DDT as response variable
fit_best_gam <- function(bird_data) {
  best_aic <- Inf
  best_model <- NULL
  
  for (spline_type in spline_types) {
    gam_model <- gam(DDT + 0.0000005 ~ s(Month, bs = spline_type), data = bird_data, family = Gamma(link = "log"), method = "REML")
    
    if (AIC(gam_model) < best_aic) {
      best_aic <- AIC(gam_model)
      best_model <- gam_model
    }
  }
  
  return(best_model)
}

bird_models_DDT <- DMP1 %>%
  group_by(LocalID) %>%
  do(model = fit_best_gam(.))

# Function to generate predictions and confidence intervals for DDT
get_predictions <- function(model, birdID) {
  new_data <- data.frame(Month = 1:12) # Months from 1 to 12
  predictions <- predict(model, newdata = new_data, se.fit = TRUE)
  ci_width <- qt(0.975, df = model$df.residual) * predictions$se.fit # 95% CI
  new_data$Predicted_DDT <- exp(predictions$fit) # Convert from log scale
  new_data$Lower_CI <- exp(predictions$fit - ci_width)
  new_data$Upper_CI <- exp(predictions$fit + ci_width)
  new_data$LocalID <- birdID
  return(new_data)
}

all_predictions <- bind_rows(mapply(get_predictions, bird_models_DDT$model, bird_models_DDT$LocalID, SIMPLIFY = FALSE))

# Plot predictions with confidence intervals and facets for DDT
plot <- ggplot() +
  # Confidence intervals (light blue ribbon)
  geom_ribbon(data = all_predictions, aes(x = Month, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "blue") +
  # Predicted fit (blue line in the foreground)
  geom_line(data = all_predictions, aes(x = Month, y = Predicted_DDT), color = "blue", size = 1.2) +
  labs(x = "Month", y = "DDT (Km)") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  facet_wrap(~ LocalID, scales = "free", ncol = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

print(plot)

# Identify the months with minima and maxima for DDT
min_max_ddt <- all_predictions %>%
  group_by(LocalID) %>%
  summarise(Min_Month = Month[which.min(Predicted_DDT)],
            Max_Month = Month[which.max(Predicted_DDT)])

# Count the occurrences for each month and type (minima and maxima)
min_max_counts_ddt <- min_max_ddt %>%
  pivot_longer(cols = c(Min_Month, Max_Month), names_to = "Type", values_to = "Month") %>%
  group_by(Month, Type) %>%
  summarise(Count = n()) %>%
  ungroup()

# Add zero counts for missing months
missing_months_ddt <- setdiff(1:12, unique(min_max_counts_ddt$Month))
missing_data_ddt <- expand.grid(Month = missing_months_ddt, Type = c("Min_Month", "Max_Month"), Count = 0)
min_max_counts_ddt <- bind_rows(min_max_counts_ddt, missing_data_ddt)

# Plot the data as side-by-side bars for DDT with annotations
plot_ddt <- ggplot(min_max_counts_ddt, aes(x = factor(Month), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + # Side-by-side bar plot
  
  # Add annotations here
  geom_text(aes(label = Count, group = Type), 
            position = position_dodge(width = 0.9), vjust = -0.5, 
            color = "black", size = 3.5) +
  
  scale_fill_manual(values = c("Min_Month" = "blue", "Max_Month" = "red")) +
  labs(x = "Month",
       y = "Count",
       title = "Occurrence of the Minima and Maxima Mean Monthly DDT for Each Bird") +
  scale_x_discrete(breaks = 1:12,
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

print(plot_ddt)

#### Model Diagnostics ####

# Now, let's visualize the model diagnostics using gam.check for each bird ID
lapply(bird_models_DMD$model, function(mod) {
  cat("Diagnostics for Bird ID:", unique(mod$data$LocalID), "\n")
  gam.check(mod)
  invisible(NULL)
}) 

## For best fit model of each bird for respective daily movement parameter i.e. DOD, DDT, DMD, SDP Model was diagnosed using visualization and model outputs ###


