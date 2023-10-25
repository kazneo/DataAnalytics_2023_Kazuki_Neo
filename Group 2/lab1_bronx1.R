library(gdata) 
install.packages('readxl')
library(readxl)

#faster xls reader but requires perl!
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="/perl/bin/perl.exe") 
#bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#bronx1<-read.xls("../Data/NYhousing/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
bronx1 <- read_excel('../Data/NYhousing/rollingsales_bronx.xls', sheet = 1, skip = 4)

colnames(bronx1) <- c('BOROUGH',	'NEIGHBORHOOD',	'BUILDING CLASS CATEGORY', 'TAX CLASS AT PRESENT',	'BLOCK',	'LOT',	
                      'EASE-MENT',	'BUILDING CLASS AT PRESENT', 'ADDRESS',	'APARTMENT NUMBER', 'ZIP CODE',	'RESIDENTIAL UNITS',
                      'COMMERCIAL UNITS',	'TOTAL UNITS', 'LAND SQUARE FEET', 'GROSS SQUARE FEET', 'YEAR BUILT',	'TAX CLASS AT TIME OF SALE',	
                      'BUILDING CLASS AT TIME OF SALE',	'SALE PRICE',	'SALE DATE')

View(bronx1)

attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
head(bronx1$`SALE PRICE`)
SALE.PRICE<-sub("\\$","",bronx1$'SALE PRICE') 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
SALE.PRICE
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$'GROSS SQUARE FEET')) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$'LAND SQUARE FEET')) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 

sum(is.na(GROSS.SQUARE.FEET))
sum(is.infinite(GROSS.SQUARE.FEET))
m1<-lm(log(SP)~log(GSF))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#
