# Part 1 a
df <- read.csv('../Data/2010EPI_data.csv', header = FALSE)

colnames(df) <- df[2, ]
df <- df[-2, ]

mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}

# AIR_E
air_e <- df$AIR_E
E <- as.numeric(as.character(air_e))
air_e <- E[!is.na(air_e)]
mean(air_e)
median(air_e)
mode(air_e)

# WATER_E
water_e <- df$WATER_E
E <- as.numeric(as.character(water_e))
water_e <- E[!is.na(water_e)]
mean(water_e)
median(water_e)
mode(water_e)

boxplot(air_e, main="AIR_E")
boxplot(water_e, main="WATER_E")


# NOX_px
nox_pt <- df$NOX_pt
E <- as.numeric(as.character(nox_pt))
nox_pt <- E[!is.na(nox_pt)]
mean(nox_pt)
median(nox_pt)
mode(nox_pt)

# SO2_pt
so2_pt <- df$SO2_pt
E <- as.numeric(as.character(so2_pt))
so2_pt <- E[!is.na(so2_pt)]
mean(so2_pt)
median(so2_pt)
mode(so2_pt)

boxplot(nox_pt, main="NOX_pt")
boxplot(so2_pt, main="SO2_pt")

# OZONE_pt box plot
ozone <- df$OZONE_pt
E <- as.numeric(as.character(ozone))
ozone <- E[!is.na(ozone)]
boxplot(ozone, main="OZONE_pt")

# WQI_pt box plot
wqi <- df$WQI_pt
E <- as.numeric(as.character(wqi))
wqi <- E[!is.na(wqi)]
boxplot(wqi, main="WQI_pt")

# ENVHEALTH
envhealth <- df$ENVHEALTH
E <- as.numeric(as.character(envhealth))
envhealth <- E[!is.na(envhealth)]
boxplot(envhealth, main="ENVHEALTH")

# ECOSYSTEM
ecosystem <- df$ECOSYSTEM
E <- as.numeric(as.character(ecosystem))
ecosystem <- E[!is.na(ecosystem)]
boxplot(ecosystem, main="ECOSYSTEM")

qqplot(envhealth,ecosystem)


# Lab 2 Part 1b
envhealth <- df$ENVHEALTH
E <- as.numeric(as.character(envhealth))
envhealth <- E[!is.na(envhealth)]

daly <- df$DALY
E <- as.numeric(as.character(daly))
daly <- E[!is.na(daly)]

air_h <- df$AIR_H
E <- as.numeric(as.character(air_h))
air_h <- E[!is.na(air_h)]

water_h <- df$WATER_H
E <- as.numeric(as.character(water_h))
water_h <- E[!is.na(water_h)]

boxplot(envhealth, daly, air_h, water_h)

lmENVH<- lm(envhealth~daly+air_h+water_h)

lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)

DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))

NEW<- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)

pENV<- predict(lmENVH,NEW,interval='prediction')
cENV<-predict(lmENVH,NEW,interval='confidence')

cENV
Model1 <- lm(AIR_E ~daly+air_h+water_h)
Model2 <- lm(CLIMATE ~DALY+AIR_H+WATER_H)

shapiro.test(cENV)
shapiro.test(envhealth)
shapiro.test(daly)
shapiro.test(air_h)
shapiro.test(water_h)

# Lab 2 Part 1c

# Read CSV data
dataFrame <- read.csv('../Data/EPI_data.csv', header = TRUE)

dataFrame

colnames(dataFrame) <- dataFrame[2, ]
dataFrame <- dataFrame[-2, ]

attatch(dataFrame)
ENVHEALTH <- dataFrame$ENVHEALTH
E <- as.numeric(as.character(ENVHEALTH))
ENVHEALTH <- E[!is.na(ENVHEALTH)]

DALY <- dataFrame$DALY
E <- as.numeric(as.character(DALY))
DALY <- E[!is.na(DALY)]

AIR_H <- dataFrame$AIR_H
E <- as.numeric(as.character(AIR_H))
AIR_H <- E[!is.na(AIR_H)]

WATER_H <- dataFrame$WATER_H
E <- as.numeric(as.character(WATER_H))
WATER_H <- E[!is.na(WATER_H)]

shapiro.test(ENVHEALTH)
shapiro.test(DALY)
shapiro.test(AIR_H)
shapiro.test(WATER_H)


# Lab 2 part 2
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
predicted_roll1 <- predict(model1, newData=newData)

ggplot(data = multiRegression, aes(x = unem, y = enroll)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(x = "Unemployment Rate (%)", y = "Fall Enrollment (ROLL)", title = "Fall Enrollment vs. Unemployment Rate") +
  theme_minimal()


# With ROLL
model2 <- lm(enroll ~ unem + hsgrad + income, data=multiRegression)

ndata <- data.frame(UNEM = 7, HGRAD = 90000, INC = 25000)  # Set INC to $25,000
predicted_roll2 <- predict(model2, newData=ndata)

ggplot(data = multiRegression, aes(x = unem, y = enroll)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(x = "Unemployment Rate (%)", y = "Fall Enrollment (ROLL)", title = "Fall Enrollment vs. Unemployment Rate") +
  theme_minimal()


# KNN Classification with abalone data
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
str(abalone)
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old')) 
abalone$rings <- as.factor(abalone$rings)

aba <- abalone
aba$sex <- NULL

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))

idx <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[idx==1,]
KNNtest <- aba[idx==2,]

KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)

# View the predicted classes
table(KNNpred)



# Clustering with IRIS

# Get iris data
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
