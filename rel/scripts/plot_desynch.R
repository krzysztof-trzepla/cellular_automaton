#!/usr/bin/env Rscript

Args <- commandArgs(trailingOnly = TRUE)
Report <- Args[1]
Cores <- Args[2]

Data <- read.csv(file=Report, sep=",", head=TRUE)

MaxDesynch <- Data[,9]
Duration <- Data[,10] / 1000

# Simulation duration against number of workers
png(filename=sprintf("desynch_%s.png", Cores))
plot(MaxDesynch, Duration, main="Simulation duration against maximal desynchronization",
     xlab="Maximal desynchronization", ylab="Duration [ms]")
lines(MaxDesynch, Duration)
dev.off()

