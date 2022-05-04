# data wrangling practice with tidyverse

# load libraries
library(tidyverse)

# read csv files
train_df <- read_csv('train.csv')
test_df <- read_csv('test.csv')

# merge data frames
train_df['label_split'] <- 'train'
test_df['label_split'] <- 'test'
test_df$SalePrice <- NA
merged_df <- rbind(train_df, test_df)

# check for missing values
# how many missing values? are they random?
summary(merged_df)

# convert Id column to character
merged_df %>%
  mutate(Id = as.character(Id))

# check how many NA are in one column
merged_df %>%
  filter(is.na(LotFrontage))

# get the sum of NA in all columns
colSums(is.na(merged_df))
