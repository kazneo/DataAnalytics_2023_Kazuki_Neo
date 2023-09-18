install.packages("csv")
EPI_data = read.csv("EPI Data/2010EPI_data.csv")

#or
#install.packages("xlsx")
#EPI_data <- read.xlsx("EPI Data/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)
#

#attach(EPI_data) 	# sets the ‘default’ object
#fix(EPI_data) 	# launches a simple data editor
#EPI_data 			# prints out values EPI_data$EPI
# tf <- is.na(EPI_data) # records True values if the value is NA
# E <- EPI_data[!tf] # filters out NA values, new array

summary(EPI_data)
fivenum(EPI_data)
help(fivenum)
stem(EPI_data)

