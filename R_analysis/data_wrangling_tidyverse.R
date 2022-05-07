# data wrangling practice with tidyverse

# load libraries
library(tidyverse)
library(naniar)
library(lubridate)

# read csv files
train_df <- read_csv('train.csv')
test_df <- read_csv('test.csv')

vis_miss(train_df)

# merge data frames
train_df['label_split'] <- 'train'
test_df['label_split'] <- 'test'
test_df$SalePrice <- NA
merged_df <- rbind(train_df, test_df)

# convert Id column to character
merged_df <- merged_df %>%
  mutate(Id = as.character(Id)) %>%
  mutate(MSSubClass = as.character(MSSubClass))
# continue checking for columns data types
# give especial attention to date columns

# MSSubClass column
table(merged_df$MSSubClass)
ggplot(merged_df, aes(MSSubClass)) +
         geom_bar()

# how MSSubClass connects to SalePrice
agg <- aggregate(SalePrice ~ MSSubClass, merged_df, mean)
agg[order(agg$SalePrice),]

# MSZoning column
merged_df %>%
  filter(is.na(MSZoning) == TRUE)
table(merged_df$MSZoning)
ggplot(merged_df, aes(MSZoning)) +
  geom_bar()
agg_MSZoning <- aggregate(SalePrice ~ MSZoning, merged_df, mean)
agg_MSZoning[order(agg_MSZoning$SalePrice),]
# replace NA values in MSZoning with most frequent values of MSZoning for each MSSUbClass
merged_df %>% group_by(MSSubClass) %>% summarize (MSZoning =names(which.max(table(MSZoning))))
merged_df <- merged_df %>%
  mutate(MSZoning = ifelse())
# replace NA with most frequent values of MSZoning for each MSSubClass
merged_df <- merged_df %>%
  mutate(MSZoning = ifelse(Id == '1916', "RM", MSZoning)) %>%
  mutate(MSZoning = ifelse(Id == '2217', "RL", MSZoning)) %>%
  mutate(MSZoning = ifelse(Id == '2905', "RM", MSZoning)) %>%
  mutate(MSZoning = ifelse(Id == '2251', "RL", MSZoning))

# dealing with missing values for column LotFrontage
boxplot(merged_df$LotFrontage)
# got correlation because distributions looked very similar
# correlation coefficient is not as high as expected, maybe because of NA values
# it seems like the best way to input values is through linear regression
cor(merged_df$SalePrice, merged_df$LotFrontage, method = "pearson", use = "complete.obs")
cor.test(merged_df$SalePrice, merged_df$LotFrontage, method = "pearson", use = "complete.obs")

# get only the features to deal with missing data
features_df <- subset(merged_df, select = -c(SalePrice, label_split))
vis_miss(features_df, cluster=TRUE)

# replace NA by 'NoAlley' in Alley column
merged_df <- merged_df %>%
  replace_na(list(Alley = "NoAlley", PoolQC = "NoPool", 
                  Fence = "NoFence", MiscFeature = "NoMisc",
                  BsmtQual = "NoBsmt", BsmtCond = "NoBsmt",
                  BsmtExposure = "NoBsmt", BsmtFinType1 = "NoBsmt",
                  BsmtFinType2 = "NoBsmt", FireplaceQu = "NoFireplace",
                  GarageType = "NoGarage", GarageFinish = "NoGarage",
                  GarageQual = "NoGarage", GarageCond = "NoGarage"))
table(merged_df$MiscFeature)

table(merged_df$MasVnrType)

# check for duplicate rows
merged_df %>%
  filter(duplicated(merged_df) == TRUE) # no duplicates found




# check for missing values
any_na(merged_df)
vis_miss(merged_df, cluster=TRUE)
# how many missing values? are they random?
summary(merged_df)


summary(merged_df$LotFrontage)
table(merged_df$MasVnrType)


# check for outliers
# create boxplot
boxplot(merged_df$SalePrice)
# create density plot
ggplot(merged_df, aes(SalePrice)) +
  geom_density()
# the distribution of SalePrice is right skewed, not normal


