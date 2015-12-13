#!/usr/bin/env Rscript

Args <- commandArgs(trailingOnly = TRUE)
N <- as.integer(Args[1])
MaxDesynch <- Args[2]

Data1 <- read.csv(file='../../data/report_1core.csv', sep=",", head=TRUE)
Data2 <- read.csv(file='../../data/report_2core.csv', sep=",", head=TRUE)
Data4 <- read.csv(file='../../data/report_4core.csv', sep=",", head=TRUE)
Data8 <- read.csv(file='../../data/report_8core.csv', sep=",", head=TRUE)

Data2 <- Data2[seq(N, NROW(Data2), by = 5),]
Data4 <- Data4[seq(N, NROW(Data4), by = 5),]
Data8 <- Data8[seq(N, NROW(Data8), by = 5),]

png(filename=sprintf("duration_%s.png", MaxDesynch))
plot(Data1[,1], round(Data1[,10] / 1000), type="l", lty=1, ylim=c(3000, 40000),
     main=sprintf("Simulation duration against number of workers\n(max desynchronization %s)", MaxDesynch),
     xlab="Number of workers", ylab="Duration [ms]")
lines(Data1[,1], round(Data2[,10] / 1000), type="l", lty=2)
lines(Data1[,1], round(Data4[,10] / 1000), type="l", lty=3)
lines(Data1[,1], round(Data8[,10] / 1000), type="l", lty=4)
legend("topright", legend = c(1, 2, 4, 8), lty=c(1,2,3,4), title="Cores")
dev.off()

