EPI_data = read.csv("../EPI Data/2010EPI_data.csv", header = FALSE)

#or
#install.packages("xlsx")
#EPI_data <- read.xlsx("../EPI Data/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!

View(EPI_data)
colnames(EPI_data) <- EPI_data[2, ]

# Remove the second row (which is now the header)
EPI_data <- EPI_data[-2, ]

attach(EPI_data)
fitted(EPI_data) 	# launches a simple data editor
DALY
E <- as.numeric(as.character(EPI))
E <- E1[!is.na(E)]

summary(E)
fivenum(as.numeric(E))
stem(as.numeric(E))

# Histogram
x <- seq(30., 95., 1)
x2 <- seq(30., 95., 2)
x3 <- seq(30, 96,  2)
hist(E1, x3, prob=TRUE)
#hist(E, seq(0, 95, 0.5), prob=TRUE)
lines(density(x3,na.rm=TRUE,bw=1.))

#Cumulative density function
plot(ecdf(E), do.points=FALSE, verticals=TRUE)

#QQ
par(pty="s")
qqnorm(E)
qqnorm(E1, E2)

#QQ fo T Dens
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqplot(EPI,DALY)


#Box Plot
boxplot(E, as.numeric(as.character(WATER_H)))
help(boxplot)
boxplot(EPI,DALY)
