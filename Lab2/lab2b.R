df <- read.csv('../Data/2010EPI_data.csv', header = TRUE)

colnames(df) <- df[2, ]
df <- df[-2, ]

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

# Lab 2 Part 1C

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
