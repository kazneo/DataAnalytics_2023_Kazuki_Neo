library(ggplot2)
library(class)

# Read Data
multiRegression <- read.csv('../Data/dataset_multipleRegression.csv', header = TRUE)
abalone <- read.csv('../Data/abalone.csv', header = TRUE)

unem <- multiRegression$UNEM
hsgrad <- multiRegression$HGRAD
enroll <- multiRegression$ROLL
income <- multiRegression$INC


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

# Regression
model1 <- lm(enroll ~ unem + hsgrad + income, data=multiRegression)

newData <- data.frame(unem = 7, hsgrad = 90000)
predicted_roll1 <- predict(model1, newData)

ggplot(data = multiRegression, aes(x = unem, y = enroll)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(x = "Unemployment Rate (%)", y = "Fall Enrollment (ROLL)", title = "Fall Enrollment vs. Unemployment Rate") +
  theme_minimal()


# With ROLL
model2 <- lm(enroll ~ unem + hsgrad + income, data=multiRegression)

ndata <- data.frame(UNEM = 7, HGRAD = 90000, INC = 25000)  # Set INC to $25,000
predicted_roll2 <- predict(model2, ndata)

ggplot(data = multiRegression, aes(x = unem, y = enroll)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(x = "Unemployment Rate (%)", y = "Fall Enrollment (ROLL)", title = "Fall Enrollment vs. Unemployment Rate") +
  theme_minimal()


# KNN Classification with abalone data

# Replace NAs with 0 in columns 1 to 7
abalone[, 1:7][is.na(abalone[, 1:7])] <- 0

# Convert columns 1 to 7 to numeric
abalone[, 1:7] <- lapply(abalone[, 1:7], as.numeric)

# Remove rows with missing values in predictors and target (Rings)
abalone <- na.omit(abalone)


set.seed(123)
idx <- sample(2, nrow(abalone), replace = TRUE, prob = c(0.7, 0.3))
train_data <- abalone[idx, ]
test_data <- abalone[-idx, ]

predictors <- train_data[, 1:7]
target <- train_data$Rings

sum_of_squared_distances <- vector('numeric', length = 10)

for (k in 1:10) {
  knn_model <- knn(train = predictors, test = predictors, cl = target, k = k)
  sum_of_squared_distances[k] <- sum((as.numeric(target) - as.numeric(knn_model))^2)
}

# Plot the sum of squared distances for each value of k
ggplot(data = data.frame(k = 1:10, sum_of_squared_distances), aes(x = k, y = sum_of_squared_distances)) +
  geom_line() +
  geom_point() +
  labs(x = 'Number of Clusters (k)', y = 'Sum of Squared Distances') +
  ggtitle('Elbow Method to Determine Optimal k') +
  theme_minimal()

k_value <- sqrt(nrow(train_data))
knn_model <- knn(train = predictors, test = test_data[, 1:7], cl = target, k = k_value)

table(predicted_age = knn_model, actual_age = test_data$Rings)


# Clustering with IRIS
data(iris)

sapply(iris[,-5], var)
summary(iris)

ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()

set.seed(300)
k.max <- 12

wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares") 
icluster <- kmeans(iris[,3:4], 2, nstart = 20)
table(icluster$cluster,iris$Species)
