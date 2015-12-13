#!/usr/bin/env Rscript

Args <- commandArgs(trailingOnly = TRUE)
Report <- Args[1]
Cores <- Args[2]

Data <- read.csv(file=Report, sep=",", head=TRUE)

Workers <- Data[,1]
Duration <- Data[,10] / 1000

# Simulation duration against number of workers
png(filename=sprintf("duration_%s.png", Cores))
plot(Workers, Duration, main="Simulation duration against number of workers",
     xlab="Number of workers", ylab="Duration [ms]")
lines(Workers, Duration)
dev.off()

