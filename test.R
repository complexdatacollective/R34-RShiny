#Test file

#install dependencies
install.packages("igraph")
install.packages("xml2")

#load libraries
library(igraph)
library(xml2)

test <- igraph::read_graph("test.graphml", format = "graphml")
test
plot(test)

#parse ego variables 

test$last_sex_overall
test$role_self
test$venue_overall
test$who_infected
test$condoms_anal_receptive
test$condoms_oral

#parse alter variables

parse.graphml("test.graphml", format = c("standard", "internal"), 
              nodes = c(), use.names = TRUE)


