# Loading packages

library(readr)
library(tidyverse)

# Loading the data
# Test data

test_set <- read_table(file = "data/UCI HAR Dataset/test/X_test.txt", col_names = F)
test_label <- read_table(file = "data/UCI HAR Dataset/test/y_test.txt", col_names = F)


# Train data

train_set <- read_table(file = "data/UCI HAR Dataset/train/X_train.txt", col_names = F)
train_label <- read_table(file = "data/UCI HAR Dataset/train/y_train.txt", col_names = F)


# Activities labels

act_labels <- read_table(file = "data/UCI HAR Dataset/activity_labels.txt", col_names = F)

# Features

features <- read_table(file = "data/UCI HAR Dataset/features.txt", col_names = F)
features2 <- str_replace(string = features$X1, pattern = "^[0-9]+", replacement = "")


# Naming columns

colnames(test_set) <- features2
colnames(train_set) <- features2

#Adding train label

test_set <- bind_cols(test_label, test_set)
train_set <- bind_cols(train_label, train_set)


# Adding subject train

subject_train <- read_table(file = "data/UCI HAR Dataset/train/subject_train.txt", col_names = F)
subject_test <- read_table(file = "data/UCI HAR Dataset/test/subject_test.txt", col_names = F)

data_test <- bind_cols(subject_test, test_set)
data_train <- bind_cols(subject_train, train_set)

names(data_test)[1] <- "subject"
names(data_train)[1] <- "subject"

names(data_test)[2] <- "activity ID"
names(data_train)[2] <- "activity ID"

# Extracting only columns containing mean and std

data_test_selected <- data_test %>%
  select(contains("subject"),
         contains("activity ID"),
         contains("mean"),
         contains("std"),
         -contains("angle"))

data_train_selected <- data_train %>%
  select(contains("subject"),
         contains("activity ID"),
         contains("mean"),
         contains("std"),
         -contains("angle"))

# Merging both sets

full_data_selected <- bind_rows(data_test_selected, data_train_selected)
full_data_selected_2 <- inner_join(x = act_labels, y = full_data_selected, by = c("X1" = "activity ID"))

names(full_data_selected_2)[1] <- "activity_id"
names(full_data_selected_2)[2] <- "activity"


# Pivoting the DF

data_tidy <- full_data_selected_2 %>%
  pivot_longer(cols = -c(activity_id, activity, subject),names_to = "variable", values_to = "measurement") %>%
  select(subject, activity_id, activity, variable, measurement)


# average of each variable grouped by activity

average_by_act <- data_tidy %>%
  group_by(activity, variable) %>%
  summarize(average = mean(measurement))

# average of each variable grouped by subject

average_by_subject <- data_tidy %>%
  group_by(subject, variable) %>%
  summarize(average = mean(measurement))