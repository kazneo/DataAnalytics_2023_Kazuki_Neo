EPI_data = read.csv("EPI Data/2010EPI_data.csv", header = FALSE)

#or
install.packages("xlsx")
EPI_data <- read.xlsx("EPI Data/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)


attach(EPI_data) 	# sets the ‘default’ object
fitted(EPI_data) 	# launches a simple data editor
data.frame <- EPI_data$V14 			# prints out values EPI_data$EPI
tf <- is.na(data.frame) # records True values if the value is NA
E <- EPI_data$V14[!tf] # filters out NA values, new array

#summary(E)
#fivenum(E)
#stem(E)

str(E)

hist(as.numeric(E),
     xlab = "EPI", 
     ylab = "Density", 
     main = "Histogram of EPI")
