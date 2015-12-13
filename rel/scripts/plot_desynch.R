#!/usr/bin/env Rscript

Args <- commandArgs(trailingOnly = TRUE)
Report <- Args[1]
Core <- Args[2]

Data <- read.csv(file=Report, sep=",", head=TRUE)
Data <- structure(list(Data[1:5,10], Data[6:10,10], Data[11:15,10], Data[16:20,10],
                      Data[21:25,10]), .Names = c(1, 2, 4, 8, 16), class = "data.frame",
                      row.names = c(NA, -5L))
Data <- round(Data / 1000)

png(filename=sprintf("desynch_%score.png", Core))
barplot(as.matrix(Data), main=sprintf("Simulation duration against number of workers (%s core)", Core),
        xlab = "Number of workers", ylab = "Duration [ms]", beside=TRUE)
legend("topright", legend = c(1, 5, 10, 20, 50), bty="n", fill=grey.colors(5), title="Max desynchronization")
dev.off()

