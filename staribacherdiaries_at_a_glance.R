library(igraph)
library(visNetwork)
library(stringi)
library(XML)

##The html_txt can parse the names and encoding
html_txt <- function(str) {
  xpathApply(htmlParse(str, asText=TRUE),
             "//body//text()", 
             xmlValue)[[1]] 
}

path="~/Documents/staribacher/"
setwd(paste0(path,"experiments/"))
#precrisis <- read_graph(paste0(path,"graphs/","staribacher_parse_tree_73-1-1_73-3-15.graphml"), format="graphml")
#postcrisis <- read_graph(paste0(path,"graphs/","staribacher_parse_tree_73-10-16_74-2-28.graphml"), format="graphml")
#precrisis <- delete.vertices(precrisis,60)
#postcrisis <- delete.vertices(postcrisis,26)

all <- read_graph(paste0(path,"graphs/","staribacher_all.graphml"),format="graphml")

#g <- precrisis
#g<- postcrisis
g <- all
g <- remove.edge.attribute(g,"visone.confirmation")
g <- remove.edge.attribute(g,"visone.direction")
g <- remove.edge.attribute(g,"persons")
g <- remove.edge.attribute(g,"weight")
g <- remove.edge.attribute(g,"visone.edgeID")
g <- remove.vertex.attribute(g,"visone.nodeID")
g <- remove.vertex.attribute(g,"source_tei")
g <- remove.vertex.attribute(g,"type")
g <- remove.graph.attribute(g,"network_pos")
g <- remove.graph.attribute(g,"network_zoom")
g <- remove.graph.attribute(g,"Network_Edge_Templates")

names <- html_txt(V(g)$name)
#Encoding(names) <- "UTF-8" #encoding to utf-8, It is optional you may avoid it
splt_txt <-strsplit(names,split="\n")[[1]]
V(g)$name <- splt_txt
  #stringi::stri_trans_general(splt_txt, "latin-ascii")


firstnames <- read.csv2(paste0(path,"graphs/staribacher_names_ids_12-19.csv"),header = TRUE, sep=",")
head(firstnames[order(firstnames$id),],100)

V(g)$firstname <- "NA"

sink(paste0(path,"experiments/notmatchingIDs2.txt"))
for(i in 1:vcount(g)){
  if(is.element(as.numeric(V(g)[i]$id), firstnames$id)){ #check if the id appears in the firstnames list
    firstname <- as.character(firstnames[firstnames$id==as.numeric(V(g)[i]$id),]$first_name)
    #if(V(g)[i]$name!=firstnames[firstnames$id==as.numeric(V(g)[i]$id),]$name){ 
      #check if name 
      #print(paste("ID:",V(g)[i]$id))
      #print(paste("In the net:",V(g)[i]$name))
      #print(paste("In the csv:",firstnames[firstnames$id==as.numeric(V(g)[i]$id),]$name))
      #print("***")
    #} 
  }else{ #if the id does not appear in the graph
    print(paste(V(g)[i]$id, V(g)[i]$name))
  }
  V(g)[i]$firstname <- firstname
}
#V(g)$firstname <- fn
sink()
which(V(g)$surname=="Mock")

V(g)$surname <- V(g)$name
V(g)$name <- paste(V(g)$surname,V(g)$firstname,sep = " ")

summary(g)
vcount(g)
ecount(g)
is.directed(g)
has.multiple(g)
which(is.loop(g))

#save(g,file = "complete_graph.RData",envir=parent.frame() )
