# install.packages('nortest')
library(nortest)
library(ggplot2)
# install.packages('gridExtra')
library(gridExtra)

df1 <- read.csv("../Data/nytimes/nyt4.csv")
df1<-df1[which(df1$Impressions>0 & df1$Clicks>0 & df1$Age>0),]
df2 <- read.csv("../Data/nytimes/nyt6.csv")
df2<-df2[which(df2$Impressions>0 & df2$Clicks>0 & df2$Age>0),]
df3 <- read.csv("../Data/nytimes/nyt8.csv")
df3<-df3[which(df3$Impressions>0 & df3$Clicks>0 & df3$Age>0),]
df4 <- read.csv("../Data/nytimes/nyt9.csv")
df4<-df4[which(df4$Impressions>0 & df4$Clicks>0 & df4$Age>0),]
df5 <- read.csv("../Data/nytimes/nyt10.csv")
df5<-df5[which(df5$Impressions>0 & df5$Clicks>0 & df5$Age>0),]
df6 <- read.csv("../Data/nytimes/nyt12.csv")
df6<-df6[which(df6$Impressions>0 & df6$Clicks>0 & df6$Age>0),]
df7 <- read.csv("../Data/nytimes/nyt22.csv")
df7<-df7[which(df7$Impressions>0 & df7$Clicks>0 & df7$Age>0),]

df1Age <- data.frame(age = df1$Age)
df2Age <- data.frame(age = df1$Age)
df3Age <- data.frame(age = df1$Age)
df4Age <- data.frame(age = df1$Age)
df5Age <- data.frame(age = df1$Age)
df6Age <- data.frame(age = df1$Age)
df7Age <- data.frame(age = df1$Age)

df1Impressions <- data.frame(impression = df1$Impressions)
df2Impressions <- data.frame(impression = df2$Impressions)
df3Impressions <- data.frame(impression = df3$Impressions)
df4Impressions <- data.frame(impression = df4$Impressions)
df5Impressions <- data.frame(impression = df5$Impressions)
df6Impressions <- data.frame(impression = df6$Impressions)
df7Impressions <- data.frame(impression = df7$Impressions)

# Boxplot
ggplot(df1Age, aes(x = "", y = age)) +
  geom_boxplot() +
  labs(y = "Age", title = "df1 Age")

ggplot(df2Age, aes(x = "", y = age)) +
  geom_boxplot() +
  labs(y = "Age", title = "df2 Age")

ggplot(df3Age, aes(x = "", y = age)) +
  geom_boxplot() +
  labs(y = "Age", title = "df3 Age")

ggplot(df4Age, aes(x = "", y = age)) +
  geom_boxplot() +
  labs(y = "Age", title = "df4 Age")

ggplot(df5Age, aes(x = "", y = age)) +
  geom_boxplot() +
  labs(y = "Age", title = "df5 Age")

ggplot(df6Age, aes(x = "", y = age)) +
  geom_boxplot() +
  labs(y = "Age", title = "df5 Age")

ggplot(df7Age, aes(x = "", y = age)) +
  geom_boxplot() +
  labs(y = "Age", title = "df7 Age")


# Combine age df into one
age <- rbind(
  data.frame(DataFrame = "NYT4 Age", Age = df1Age$age),
  data.frame(DataFrame = "NYT6 Age", Age = df2Age$age),
  data.frame(DataFrame = "NYT8 Age", Age = df3Age$age),
  data.frame(DataFrame = "NYT9 Age", Age = df4Age$age),
  data.frame(DataFrame = "NYT10 Age", Age = df5Age$age),
  data.frame(DataFrame = "NYT12 Age", Age = df6Age$age),
  data.frame(DataFrame = "NYT22 Age", Age = df7Age$age)
)

# Create Box Plot
ggplot(age, aes(x = DataFrame, y = Age)) +
  geom_boxplot() +
  labs(x = "Data Frame", y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


impression <- rbind(
  data.frame(DataFrame = "NYT4 impression", Impression = df1Impressions$impression),
  data.frame(DataFrame = "NYT6 impression", Impression = df2Impressions$impression),
  data.frame(DataFrame = "NYT8 impression", Impression = df3Impressions$impression),
  data.frame(DataFrame = "NYT9 impression", Impression = df4Impressions$impression),
  data.frame(DataFrame = "NYT10 impression", Impression = df5Impressions$impression),
  data.frame(DataFrame = "NYT12 impression", Impression = df6Impressions$impression),
  data.frame(DataFrame = "NYT22 impression", Impression = df7Impressions$impression)
)

# Create Box Plot
ggplot(impression, aes(x = DataFrame, y = Impression)) +
  geom_boxplot() +
  labs(x = "Data Frame", y = "Impression") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Histogram

# NYT 4
# Age
ggplot(df1Age, aes(x = age)) + geom_histogram(binwidth = 3, fill = "blue", color = "black") + labs(x = "Age", y = "Frequency", title = "Age Histogram")

# Impression
ggplot(df1Impressions, aes(x = impression)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Impression", y = "Frequency", title = "Impression Histogram")

# NYT 6
ggplot(df2Age, aes(x = age)) + geom_histogram(binwidth = 3, fill = "blue", color = "black") + labs(x = "Age", y = "Frequency", title = "Age Histogram")
ggplot(df2Impressions, aes(x = impression)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Impression", y = "Frequency", title = "Impression Histogram")

# NYT 8
ggplot(df3Age, aes(x = age)) + geom_histogram(binwidth = 3, fill = "blue", color = "black") + labs(x = "Age", y = "Frequency", title = "Age Histogram")
ggplot(df3Impressions, aes(x = impression)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Impression", y = "Frequency", title = "Impression Histogram")

# NYT 9
ggplot(df4Age, aes(x = age)) + geom_histogram(binwidth = 3, fill = "blue", color = "black") + labs(x = "Age", y = "Frequency", title = "Age Histogram")
ggplot(df4Impressions, aes(x = impression)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Impression", y = "Frequency", title = "Impression Histogram")

# NYT 10
ggplot(df5Age, aes(x = age)) + geom_histogram(binwidth = 3, fill = "blue", color = "black") + labs(x = "Age", y = "Frequency", title = "Age Histogram")
ggplot(df5Impressions, aes(x = impression)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Impression", y = "Frequency", title = "Impression Histogram")

# NYT 12
ggplot(df6Age, aes(x = age)) + geom_histogram(binwidth = 3, fill = "blue", color = "black") + labs(x = "Age", y = "Frequency", title = "Age Histogram")
ggplot(df6Impressions, aes(x = impression)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Impression", y = "Frequency", title = "Impression Histogram")

# NYT 22
ggplot(df7Age, aes(x = age)) + geom_histogram(binwidth = 3, fill = "blue", color = "black") + labs(x = "Age", y = "Frequency", title = "NYT 22 Age Histogram")
ggplot(df7Impressions, aes(x = impression)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Impression", y = "Frequency", title = "NYT 22 Impression Histogram")


# Normalization

# NYT 4

# AD Test
df1age <- unique(df1Age$age)

df1_age_ad_test <- ad.test(df1age)
df1_age_ad_test

# KS Test
df1age_ks_test <- ks.test(df1age, "pnorm", mean = mean(df1age), sd = sd(df1age))
df1age_ks_test


# Shapiro-Wilk Test
set.seed(123)
df1age <- sample(df1Age$age, 50)

df1_age_shapiro_test <- shapiro.test(df1age)
df1_age_shapiro_test

# AD Test
df1impression <- unique(df1Impressions$impression)
df1impression <- df1impression[!is.na(df1impression)]

df1_impression_ad_test <- ad.test(df1impression)
df1_impression_ad_test

# KS Test
df1impression_ks_test <- ks.test(df1impression, "pnorm", mean = mean(df1impression), sd = sd(df1impression))
df1impression_ks_test


# Shapiro-Wilk Test
set.seed(123)
df1impression <- sample(df1Impressions$impression, 50)

df1_impression_shapiro_test <- shapiro.test(df1impression)
df1_impression_shapiro_test



# Q-Q plot
ggplot(df1Age, aes(sample = age)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 4 Age")

ggplot(df1Impressions, aes(sample = impression)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 4 Impression")


# NYT 6

# AD Test
df2age <- unique(df2Age$age)

df2_age_ad_test <- ad.test(df2age)
df2_age_ad_test

# KS Test
df2age_ks_test <- ks.test(df2age, "pnorm", mean = mean(df2age), sd = sd(df2age))
df2age_ks_test


# Shapiro-Wilk Test
set.seed(123)
df2age <- sample(df2Age$age, 50)

df2_age_shapiro_test <- shapiro.test(df2age)
df2_age_shapiro_test

# AD Test
df2impression <- unique(df2Impressions$impression)

df2_impression_ad_test <- ad.test(df2impression)
df2_impression_ad_test

# KS Test
df2impression_ks_test <- ks.test(df2impression, "pnorm", mean = mean(df2impression), sd = sd(df2impression))
df2impression_ks_test


# Shapiro-Wilk Test
set.seed(123)
df2impressions <- sample(df2Impressions$impression, 50)

df2_impression_shapiro_test <- shapiro.test(df2impressions)
df2_impression_shapiro_test



# Q-Q plot
ggplot(df2Age, aes(sample = age)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 6 Age")

ggplot(df1Impressions, aes(sample = impression)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 6 Impression")


# NYT 8

# AD Test
df3age <- unique(df3Age$age)

df3_age_ad_test <- ad.test(df3age)
df3_age_ad_test

# KS Test
df3age_ks_test <- ks.test(df3age, "pnorm", mean = mean(df3age), sd = sd(df3age))
df3age_ks_test


# Shapiro-Wilk Test
set.seed(123)
df3age <- sample(df3Age$age, 50)

df3_age_shapiro_test <- shapiro.test(df3age)
df3_age_shapiro_test

# AD Test
df3impression <- unique(df3Impressions$impression)

df3_impression_ad_test <- ad.test(df3impression)
df3_impression_ad_test

# KS Test
df3impression_ks_test <- ks.test(df1impression, "pnorm", mean = mean(df3impression), sd = sd(df3impression))
df3impression_ks_test


# Shapiro-Wilk Test
set.seed(123)
df3impression <- sample(df3Impressions$impression, 50)

df3_impression_shapiro_test <- shapiro.test(df3impression)
df3_impression_shapiro_test



# Q-Q plot
ggplot(df3Age, aes(sample = age)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 8 Age")

ggplot(df3Impressions, aes(sample = impression)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 8 Impression")


# NYT 9

# AD Test
df4age <- unique(df4Age$age)

df4_age_ad_test <- ad.test(df4age)
df4_age_ad_test

# KS Test
df4age_ks_test <- ks.test(df4age, "pnorm", mean = mean(df4age), sd = sd(df4age))
df4age_ks_test


# Shapiro-Wilk Test
set.seed(123)
df4age <- sample(df4Age$age, 50)

df4_age_shapiro_test <- shapiro.test(df4age)
df4_age_shapiro_test

# AD Test
df4impression <- unique(df4Impressions$impression)

df4_impression_ad_test <- ad.test(df4impression)
df4_impression_ad_test

# KS Test
df4impression_ks_test <- ks.test(df4impression, "pnorm", mean = mean(df4impression), sd = sd(df4impression))
df4impression_ks_test


# Shapiro-Wilk Test
set.seed(123)
df4impressions <- sample(df4Impressions$impression, 50)

df4_impression_shapiro_test <- shapiro.test(df4impressions)
df4_impression_shapiro_test



# Q-Q plot
ggplot(df4Age, aes(sample = age)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 9 Age")

ggplot(df1Impressions, aes(sample = impression)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 9 Impression")


# NYT 10

# AD Test
df5age <- unique(df5Age$age)

df5_age_ad_test <- ad.test(df5age)
df5_age_ad_test

# KS Test
df5age_ks_test <- ks.test(df3age, "pnorm", mean = mean(df3age), sd = sd(df3age))
df5age_ks_test


# Shapiro-Wilk Test
set.seed(123)
df5age <- sample(df5Age$age, 50)

df5_age_shapiro_test <- shapiro.test(df5age)
df5_age_shapiro_test

# AD Test
df5impression <- unique(df5Impressions$impression)
df5impression <- df5impression[!is.na(df5impression)]

df5_impression_ad_test <- ad.test(df5impression)
df5_impression_ad_test

# KS Test
df5impression_ks_test <- ks.test(df5impression, "pnorm", mean = mean(df5impression), sd = sd(df5impression))
df5impression_ks_test


# Shapiro-Wilk Test
set.seed(123)

df5_impression_shapiro_test <- shapiro.test(df5impression)
df5_impression_shapiro_test



# Q-Q plot
ggplot(df5Age, aes(sample = age)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 10 Age")

ggplot(df1Impressions, aes(sample = impression)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 10 Impression")


# NYT 12

# AD Test
df6age <- unique(df6Age$age)

df6_age_ad_test <- ad.test(df6age)
df6_age_ad_test

# KS Test
df6age_ks_test <- ks.test(df6age, "pnorm", mean = mean(df6age), sd = sd(df6age))
df6age_ks_test


# Shapiro-Wilk Test
set.seed(123)
df6age <- sample(df6Age$age, 50)

df6_age_shapiro_test <- shapiro.test(df6age)
df6_age_shapiro_test

# AD Test
df6impression <- unique(df6Impressions$impression)

df6_impression_ad_test <- ad.test(df6impression)
df6_impression_ad_test

# KS Test
df6impression_ks_test <- ks.test(df6impression, "pnorm", mean = mean(df2impression), sd = sd(df6impression))
df6impression_ks_test


# Shapiro-Wilk Test
set.seed(123)
df6impressions <- sample(df6Impressions$impression, 50)

df6_impression_shapiro_test <- shapiro.test(df6impressions)
df6_impression_shapiro_test



# Q-Q plot
ggplot(df6Age, aes(sample = age)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 12 Age")

ggplot(df6Impressions, aes(sample = impression)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 12 Impression")


# NYT 22

# AD Test
df7age <- unique(df7Age$age)

df7_age_ad_test <- ad.test(df3age)
df7_age_ad_test

# KS Test
df7age_ks_test <- ks.test(df3age, "pnorm", mean = mean(df7age), sd = sd(df7age))
df7age_ks_test


# Shapiro-Wilk Test
set.seed(123)
df7age <- sample(df3Age$age, 50)

df7_age_shapiro_test <- shapiro.test(df7age)
df7_age_shapiro_test

# AD Test
df7impression <- unique(df7Impressions$impression)

df7_impression_ad_test <- ad.test(df7impression)
df7_impression_ad_test

# KS Test
df7impression_ks_test <- ks.test(df1impression, "pnorm", mean = mean(df7impression), sd = sd(df7impression))
df7impression_ks_test


# Shapiro-Wilk Test
set.seed(123)
df7impression <- sample(df7Impressions$impression, 50)

df7_impression_shapiro_test <- shapiro.test(df7impression)
df7_impression_shapiro_test



# Q-Q plot
ggplot(df7Age, aes(sample = age)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 22 Age")

ggplot(df7Impressions, aes(sample = impression)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "NYT 22 Impression")

# Create data frames to store the results of the tests
shapiro_results_age <- data.frame(
  Dataset = c("NYT 4", "NYT 6", "NYT 8", "NYT 9", "NYT 10", "NYT 12", "NYT 22"),
  Shapiro_Wilk_P_Value = c(df1_age_shapiro_test$p.value, df2_age_shapiro_test$p.value, df3_age_shapiro_test$p.value, df4_age_shapiro_test$p.value, df5_age_shapiro_test$p.value, df6_age_shapiro_test$p.value, df7_age_shapiro_test$p.value)
)
shapiro_results_age

ad_results_age <- data.frame(
  Dataset = c("NYT 4", "NYT 6", "NYT 8", "NYT 9", "NYT 10", "NYT 12", "NYT 22"),
  Anderson_Darling_P_Value = c(df1_age_ad_test$p.value, df2_age_ad_test$p.value, df3_age_ad_test$p.value, df4_age_ad_test$p.value, df5_age_ad_test$p.value, df6_age_ad_test$p.value, df7_age_ad_test$p.value)
)
ad_results_age

ks_results_age <- data.frame(
  Dataset = c("NYT 4", "NYT 6", "NYT 8", "NYT 9", "NYT 10", "NYT 12", "NYT 22"),
  KS_P_Value_Age = c(df1age_ks_test$p.value, df2age_ks_test$p.value, df3age_ks_test$p.value, df4age_ks_test$p.value, df5age_ks_test$p.value, df6age_ks_test$p.value, df7age_ks_test$p.value)
)

ks_results_age

shapiro_results_impression <- data.frame(
  Dataset = c("NYT 4", "NYT 6", "NYT 8", "NYT 9", "NYT 10", "NYT 12", "NYT 22"),
  Shapiro_Wilk_P_Value = c(df1_impression_shapiro_test$p.value, df2_impression_shapiro_test$p.value, df3_impression_shapiro_test$p.value, df4_impression_shapiro_test$p.value, df5_impression_shapiro_test$p.value, df6_impression_shapiro_test$p.value, df7_impression_shapiro_test$p.value)
)

shapiro_results_impression

ad_results_impression <- data.frame(
  Dataset = c("NYT 4", "NYT 6", "NYT 8", "NYT 9", "NYT 10", "NYT 12", "NYT 22"),
  Anderson_Darling_P_Value = c(df1_impression_ad_test$p.value, df2_impression_ad_test$p.value, df3_impression_ad_test$p.value, df4_impression_ad_test$p.value, df5_impression_ad_test$p.value, df6_impression_ad_test$p.value, df7_impression_ad_test$p.value)
)

ad_results_impression


ks_results_impression <- data.frame(
  Dataset = c("NYT 4", "NYT 6", "NYT 8", "NYT 9", "NYT 10", "NYT 12", "NYT 22"),
  KS_P_Value_Impression = c(df1impression_ks_test$p.value, df2impression_ks_test$p.value, df3impression_ks_test$p.value, df4impression_ks_test$p.value, df5impression_ks_test$p.value, df6impression_ks_test$p.value, df7impression_ks_test$p.value)
)
ks_results_impression


# ECDF

# Create ECDF plots for 'age'
df1_ecdf_age <- ecdf(df1Age$age)
df1_ecdf_age_plot <- ggplot(data.frame(x = seq(min(df1Age$age), max(df1Age$age), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "NYT 4 Age ECDF", x = "Age", y = "ECDF")

df2_ecdf_age <- ecdf(df2Age$age)
df2_ecdf_age_plot <- ggplot(data.frame(x = seq(min(df2Age$age), max(df2Age$age), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "NYT 6 Age ECDF", x = "Age", y = "ECDF")

df3_ecdf_age <- ecdf(df1Age$age)
df3_ecdf_age_plot <- ggplot(data.frame(x = seq(min(df3Age$age), max(df3Age$age), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "NYT 8 Age ECDF", x = "Age", y = "ECDF")

df4_ecdf_age <- ecdf(df1Age$age)
df4_ecdf_age_plot <- ggplot(data.frame(x = seq(min(df4Age$age), max(df4Age$age), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "NYT 9 Age ECDF", x = "Age", y = "ECDF")

df5_ecdf_age <- ecdf(df5Age$age)
df5_ecdf_age_plot <- ggplot(data.frame(x = seq(min(df5Age$age), max(df5Age$age), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "NYT 10 Age ECDF", x = "Age", y = "ECDF")

df6_ecdf_age <- ecdf(df6Age$age)
df6_ecdf_age_plot <- ggplot(data.frame(x = seq(min(df6Age$age), max(df6Age$age), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "NYT 12 Age ECDF", x = "Age", y = "ECDF")

df7_ecdf_age <- ecdf(df1Age$age)
df7_ecdf_age_plot <- ggplot(data.frame(x = seq(min(df7Age$age), max(df7Age$age), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "NYT 22 Age ECDF", x = "Age", y = "ECDF")

grid.arrange(
  df1_ecdf_age_plot, df2_ecdf_age_plot, df3_ecdf_age_plot,
  df4_ecdf_age_plot, df5_ecdf_age_plot, df6_ecdf_age_plot, df7_ecdf_age_plot,
  ncol = 3
)

# Create ECDF plots for 'Impression'
df1_ecdf_impression <- ecdf(df1Impressions$impression)
df1_ecdf_impression_plot <- ggplot(data.frame(x = seq(min(df1Impressions$impression), max(df1Impressions$impression), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "NYT 4 Impression ECDF", x = "Impression", y = "ECDF")

df2_ecdf_impression <- ecdf(df1Impressions$impression)
df2_ecdf_impression_plot <- ggplot(data.frame(x = seq(min(df2Impressions$impression), max(df2Impressions$impression), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "NYT 6 Impression ECDF", x = "Impression", y = "ECDF")

df3_ecdf_impression <- ecdf(df1Impressions$impression)
df3_ecdf_impression_plot <- ggplot(data.frame(x = seq(min(df3Impressions$impression), max(df3Impressions$impression), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "NYT 8 Impression ECDF", x = "Impression", y = "ECDF")

df4_ecdf_impression <- ecdf(df4Impressions$impression)
df4_ecdf_impression_plot <- ggplot(data.frame(x = seq(min(df4Impressions$impression), max(df4Impressions$impression), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "NYT 9 Impression ECDF", x = "Impression", y = "ECDF")

df5_ecdf_impression <- ecdf(df5Impressions$impression)
df5_ecdf_impression_plot <- ggplot(data.frame(x = seq(min(df5Impressions$impression), max(df5Impressions$impression), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "NYT 10 Impression ECDF", x = "Impression", y = "ECDF")

df6_ecdf_impression <- ecdf(df6Impressions$impression)
df6_ecdf_impression_plot <- ggplot(data.frame(x = seq(min(df6Impressions$impression), max(df6Impressions$impression), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "NYT 12 Impression ECDF", x = "Impression", y = "ECDF")

df7_ecdf_impression <- ecdf(df1Impressions$impression)
df7_ecdf_impression_plot <- ggplot(data.frame(x = seq(min(df7Impressions$impression), max(df7Impressions$impression), length.out = 100)), aes(x)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "NYT 22 Impression ECDF", x = "Impression", y = "ECDF")

grid.arrange(df1_ecdf_impression_plot, df2_ecdf_impression_plot, 
             df3_ecdf_impression_plot, df4_ecdf_impression_plot,
             df5_ecdf_impression_plot, df6_ecdf_impression_plot,
             df7_ecdf_impression_plot, 
             ncol = 3)

