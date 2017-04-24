#now want to visualize the families/parent offspring pairs. Use igraph for this, it will create networks
#create a matrix of nodes
# Set up working space ----------------------------------------------------
setwd("~/Documents/GradSchool/parentage")
#source("~/Documents/GradSchool/parentage/readGenepop_space.R")
suppressMessages(library(dplyr))
library(igraph)
fullsib <- read.table(file= "174HQloci_adult_sep_2013.FullSibDyad10.txt", header= TRUE)
halfsib <- read.table(file= "174HQloci_adult_sep_2013.HalfSibDyad10.txt", header= TRUE)	
pairs <- read.table(file= "174HQloci_adult_sep_2013.Paternity.txt", header= TRUE)	

#first get rid of low probability matches
fullsib <-filter(fullsib, Probability > 0.95)
halfsib <- filter(halfsib, Probability > 0.95)
edgesp <- data.frame(select(pairs, OffspringID, InferredDad1), stringsAsFactors = FALSE)
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
nodes2 <- select(pairs, InferredDad1)
nodes3 <- select(fullsib, sib1)
nodes4 <- select(fullsib, sib2)
nodes5 <- select(halfsib, hsib1)
nodes6 <- select(halfsib, hsib2)
nodesa <- full_join(nodes1, nodes2, by=c(OffspringID = "InferredDad1"))
nodesb <-full_join(nodesa, nodes3, by=c(OffspringID = "sib1"))
nodesc <-full_join(nodesb, nodes4, by=c(OffspringID = "sib2"))
nodesd <-full_join(nodesc, nodes5, by=c(OffspringID = "hsib1"))
nodese <-full_join(nodesd, nodes6, by=c(OffspringID = "hsib2"))


#remove duplicate vertexs and nodes
nodes <- distinct(nodesc)
edges <- distinct(edges1)

#optional: add size classification for vertices, to see who is big and small and so who might be adult/juv. Run code below and then remake nodes and edges matrics
#want to add sizes from only 2012 ligation ids
#now that I have adult IDs, need to connect these to ligation IDs
#YEAR 2012
c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c6 <- collect(left_join(c4, c3, by = "digest_id"))
ids <- inner_join(c5, c6, by= "sample_id")
size <-ids %>% filter(date >= as.Date("2012-04-12") & date <= as.Date("2012-07-31"))

sizes <- left_join(size, clowns, by="sample_id") %>% select(ligation_id.x, size)
nodesS <- inner_join(nodes, sizes, by=c(OffspringID= "ligation_id.x"))
nodesS
#well, I have no idea why but there are two sizes recorded for four sample IDs, so there are 4 repeats and you can't run the graph analysis with repeats. So manually write out the table ans
nodesS$ratio <- NA

#for (i in 1:nrow(nodesS))
#{
  if ((nodesS$size[i] < 8))
  {
    nodesS$ratio[i] <- "small"
  }
}

#for (i in 1:nrow(nodesS))
#{
  if ((nodesS$size[i] >= 8))
  {
    nodesS$ratio[i] <- "big"
  }
}

#make into igraph matrix
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
E(net)$color <- as.factor(E(net)$type)
V(net)$color <- as.factor(V(net)$ratio)
#plot
plot(net, edge.arrow.size=.4, vertex.label=V(net)$nodes, vertex.size=4, 

     vertex.frame.color="gray", vertex.label.color="black", vertex.label.cex=0.2, vertex.label.dist=0, label.x=edges)
	 
net


