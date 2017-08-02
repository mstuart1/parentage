#now want to visualize the families/parent offspring pairs. Use igraph for this, it will create networks
#create a matrix of nodes
# Set up working space ----------------------------------------------------
setwd("~/Documents/GradSchool/parentage")
#source("~/Documents/GradSchool/parentage/readGenepop_space.R")
suppressMessages(library(dplyr))
library(igraph)
fullsib <- read.table(file= "175HQloci_adults_sep_2015.FullSibdyad.txt", header= TRUE)
halfsib <- read.table(file= "175HQloci_adults_sep_2015.HalfSibdyad.txt", header= TRUE)	
pairs <- read.table(file= "175HQloci_adults_sep_2015.Maternity.txt", header= TRUE)	

#first get rid of low probability matches
fullsib <-data.frame(filter(fullsib, Probability > 0.98), stringsAsFactors = FALSE)
halfsib <- data.frame(filter(halfsib, Probability > 0.98), stringsAsFactors = FALSE)
pairs <- data.frame(filter(pairlatlon, ProbMum1 > 0.98), stringsAsFactors = FALSE)

edgesp <- data.frame(select(pairs, OffspringID, InferredMum1), stringsAsFactors = FALSE)
edgesp$type <- "parent"
edgesfs <- data.frame(select(fullsib, sib1, sib2), stringsAsFactors = FALSE)
edgesfs$type <- "fullsib"
edgeshs <- select(halfsib, hsib1, hsib2)
edgeshs$type <- "halfsib"
names(edgesp) <- c("one", "two", "type")
names(edgesfs) <- c("one", "two", "type")
names(edgeshs) <- c("one", "two", "type")
edges1 <- rbind(edgesp, edgesfs)
edges2 <- rbind(edges1, edgeshs)


nodes1 <- select(pairs, OffspringID)
nodes2 <- select(pairs, InferredMum1)
nodes3 <- select(fullsib, sib1)
nodes4 <- select(fullsib, sib2)
nodes5 <- select(halfsib, hsib1)
nodes6 <- select(halfsib, hsib2)
nodesa <- full_join(nodes1, nodes2, by=c(OffspringID = "InferredMum1"))
nodesb <-full_join(nodesa, nodes3, by=c(OffspringID = "sib1"))
nodesc <-full_join(nodesb, nodes4, by=c(OffspringID = "sib2"))
nodesd <-full_join(nodesc, nodes5, by=c(OffspringID = "hsib1"))
nodese <-full_join(nodesd, nodes6, by=c(OffspringID = "hsib2"))


#remove duplicate vertexs and nodes
nodes <- distinct(nodese)
edges <- distinct(edges2)

#make into igraph matrix
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
E(net)$color <- as.factor(E(net)$type)
#plot
plot(net, edge.arrow.size=.4, vertex.label=V(net)$nodes, vertex.size=2, edge.color=c("green", "orange", "blue")[(E(net)$type=="parent")+2],

     vertex.frame.color="gray", vertex.label.color="black", vertex.label.cex=0.1, vertex.label.dist=0)
	 
net


