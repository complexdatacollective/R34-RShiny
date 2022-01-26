#test to see what needs to be done to data file to display


library(igraph)
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

library(xml2)
parse.graphml("test.graphml", format = c("standard", "internal"), 
              nodes = c(), use.names = TRUE)
