library(XML)
rm(list=ls())
tables = readHTMLTable("http://www.aoml.noaa.gov/hrd/hurdat/comparison_table.html", as.data.frame=F)
hurdat = data.frame(Year=as.numeric(tables[[3]]$Year[1:164]),
                    RevisedNamedStorms=as.numeric(tables[[3]]$RevisedNamedStorms[1:164]),
                    RevisedHurricanes=as.numeric(tables[[3]]$RevisedHurricanes[1:164]),
                    RevisedMajorHurricanes=as.numeric(tables[[3]]$RevisedMajorHurricanes[1:164]),
                    RevisedACE=as.numeric(tables[[3]]$RevisedACE[1:164]))
write.csv(hurdat, file="hurdat.csv", row.names=F)
