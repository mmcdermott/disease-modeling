# make it output to pdf
pdf("Rplots.pdf")
# read in data from csv file
data <- read.csv("SIRdata.csv")
# plot the data
yrange <- range(data$S, data$I, data$R)
plot(data$S, col='green', type='l', ylim=yrange)
lines(data$I, col='red')
lines(data$R, col='blue')
