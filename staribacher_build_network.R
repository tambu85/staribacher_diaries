
library(igraph)
library(pals)
library(RColorBrewer)
library(reshape2)
library(plotly)
library(viridis)

path="~/Documents/staribacher/"
setwd(paste0(path,"experiments/"))
load("~/Documents/staribacher/graphs/complete_graph.RData")


E(g)$year=format(as.Date(E(g)$date),'%Y')
V(g)$membership <- clusters(g)$membership

#giant component (removing isolated group of 1-4 nodes)
gc <- induced_subgraph(g, V(g)[V(g)$membership==1])
summary(gc)
vcount(gc)
ecount(gc)
is.directed(gc)
has.multiple(gc)
all(!is.loop(gc)) # if true, no loops
#gc <- as.undirected(gc)

g_toplot <- as.undirected(gc)
  #simplify(as.undirected(gc), mode = "collapse"))

layout_big=layout_with_fr(g_toplot,dim=2,
                      niter=5000, 
                      grid="nogrid")

plot(g_toplot, 
     #layout=layout_base,
     layout=layout_big,
     vertex.size= 0.5,
     #vertex.shape=ifelse(V(y_int_g)$bridge, "csquare", "circle"),
     #edge.arrow.size=0.001,
     vertex.frame.color = adjustcolor("black", alpha.f = 0.3),
     vertex.color = adjustcolor("red", alpha.f = 0.5),
     vertex.label.dist=0.3, 
     vertex.label.family="Helvetica",
     edge.width = 0.5,
     #edge.curved=0.3, 
     edge.color= adjustcolor("grey", alpha.f = 0.35),
     vertex.label=NA)


years <- sort(as.numeric(unique(E(gc)$year)))
Glist <- list()

#extract a subgraph for each year 
for(i in 1:length(years)){
  y = years[i]
  print(y)
  y_links_todel <- E(gc)[E(gc)$year!=as.character(y)] 
  y_g <- delete_edges(gc, y_links_todel)
  V(y_g)$deg <- degree(y_g)
  V(y_g)$membership <- clusters(y_g)$membership
  y_gc <- induced_subgraph(y_g, V(y_g)[V(y_g)$membership==1])
  y_gc <- as.undirected(y_gc)
  E(y_gc)$w <- 1
  y_sg <- igraph::simplify(y_gc,remove.multiple = TRUE, 
                           edge.attr.comb = list(w=function(x) sum(x),
                                                 date="ignore",words="ignore",year="ignore"))
  y_sg$year = y
  Glist[[i]] <- y_sg
} 



indextoplot<-11
plot(Glist[[indextoplot]],
     layout=layout_on_sphere(Glist[[indextoplot]]),
     vertex.size=2,
     vertex.label=ifelse(V(Glist[[indextoplot]])$deg>50, V(Glist[[indextoplot]])$name, NA),
     vertex.label.cex = 0.2,
     vertex.label.color = "black",
     vertex.frame.color = NA,
     vertex.color = adjustcolor("orange", alpha.f = 0.35),
     vertex.label.dist=0.2, 
     edge.width = 0.05+0.1*E(Glist[[indextoplot]])$w,
     #edge.curved=0.3, 
     edge.color=  adjustcolor("lightgrey", alpha.f = 0.5),
     main=years[indextoplot])

#computing common nodes
#starting from 1971 since 1970 has only 36 nodes
common_nodes <- V(Glist[[2]])$name
cc <- vector()
assort <- vector()
for(i in 2:length(years)){
  print(years[i])
  print(vcount(Glist[[i]]))
  cc[i-1]=transitivity(Glist[[i]])
  assort[i-1]=assortativity_degree(Glist[[i]])
  print(paste("clustering coefficient:",cc[i-1]))
  print(paste("assortativity:",assort[i-1]))
  common_nodes <- intersect(common_nodes,V(Glist[[i]])$name)
  num_common_nodes <- length(common_nodes)
  print("*****")
}

mean(assort)
mean(cc)

intersectGlist <- list()
y_int_g <- induced.subgraph(Glist[[2]],V(Glist[[2]])[common_nodes]) #1971
layout_base <- layout_with_fr(y_int_g,
                              dim=2,
                              niter=500000, 
                              start.temp = sqrt(vcount(y_int_g)), 
                              grid="nogrid", 
                              weights=NA)
#  norm_coords(layout.kamada.kawai(y_int_g), xmin = -1, xmax = 1, ymin = -1, ymax = 1)
#  layout_with_fr(y_int_g, niter = 1000)
#  layout_nicely(y_int_g, dim=2)
#  layout.kamada.kawai(y_int_g)
#  layout_with_kk(y_int_g,kkconst = 1.5*vcount(y_int_g))
#  layout_on_sphere(y_int_g)
#  layout.circle(y_int_g)
 
mytable<- matrix(nrow=length(years)-1,ncol=6)
colnames(mytable)=c("nodes","edges","max_deg","isolated","assortativity","transitivity")
rownames(mytable)=years[-1]
selected <- list()

communities_mat <- matrix(NA,nrow=num_common_nodes,ncol=length(years)-1)
colnames(communities_mat)<-years[2:length(years)]
rownames(communities_mat)<-V(y_int_g)$name


#plot 
for(i in 2:length(years)){
  print(years[i])
  y_int_g <- induced.subgraph(Glist[[i]],V(Glist[[i]])[common_nodes])
  V(y_int_g)$membership <- clusters(y_int_g)$membership
  #y_int_g <- induced_subgraph(y_int_g, V(y_int_g)[V(y_int_g)$membership==1])
  V(y_int_g)$deg <- degree(y_int_g)
  #print(length(V(y_int_g)[V(y_int_g)$deg==0]))
  V(y_int_g)$bridge <- 0
  V(y_int_g)$btw <- betweenness(as.undirected(y_int_g))
  btw_high_values <- sort(V(y_int_g)$btw, decreasing = TRUE)[1:20]
  bridges <-subset(V(y_int_g),V(y_int_g)$btw>=min(btw_high_values))
  selected <- unique(c(selected,V(y_int_g)[bridges]))
  V(y_int_g)[bridges]$bridge <- 1
  V(y_int_g)$community <- membership(cluster_louvain(as.undirected(y_int_g), weights=E(y_int_g)$w))
  if(V(y_int_g)[1]$community != 1){
    kreisky_comm <- V(y_int_g)[1]$community
    V(y_int_g)[V(y_int_g)$community==1]$community=0
    V(y_int_g)[V(y_int_g)$community==kreisky_comm]$community=1
    V(y_int_g)[V(y_int_g)$community==0]$community=kreisky_comm
  }
  communities_mat[,as.character(years[i])] <- V(y_int_g)$community
  print(paste("NUM COMMUNITIES:",max(V(y_int_g)$community)))
  #palette <- alphabet(n=length(unique(V(y_int_g)$community)))
  palette <- brewer.paired(n=length(unique(V(y_int_g)$community)))
  V(y_int_g)$color <- palette[V(y_int_g)$community]
  E(y_int_g)$community <- 0 
  E(y_int_g)$color <- "light grey"
  for(ei in 1:ecount(y_int_g)){
    e <- E(y_int_g)[ei]
    from <- ends(y_int_g,e)[1]
    to <- ends(y_int_g,e)[2]
    if(V(y_int_g)[from]$community == V(y_int_g)[to]$community){
      E(y_int_g)[e]$community=V(y_int_g)[from]$community
      E(y_int_g)[e]$color=V(y_int_g)[from]$color
    }
  }
  
 #pdf(file=paste0(path,"experiments/plots/intersection/poster/",years[i],"_intersection_fr",".pdf"))
 #par(bg="black",col.main="lavender")
  plot(y_int_g, layout=layout_base,
       vertex.size= 4+ 0.2*V(y_int_g)$deg,
       #vertex.shape=ifelse(V(y_int_g)$bridge, "csquare", "circle"),
       #edge.arrow.size=0.001,
       vertex.label.cex = 0.4,
       vertex.label.color = adjustcolor(V(y_int_g)$color, alpha.f = 1),
       vertex.frame.color = NA,
       vertex.color = adjustcolor(V(y_int_g)$color, alpha.f = 0.75),
       vertex.label.dist=0.5, 
       edge.width = 1.5+0.1*E(y_int_g)$w,
       #edge.curved=0.3, 
       edge.color= adjustcolor(E(y_int_g)$color, alpha.f = 0.25), #adjustcolor("lightgrey", alpha.f = 0.05),
       #vertex.label=ifelse(V(y_int_g)$bridge, V(y_int_g)$name, NA),
       vertex.label=NA,
       main=paste("Year:",years[i]) )
  dev.off()
  
  intersectGlist[[i]] <- y_int_g
  name <- paste0(path,"graphs/intersection_",years[i])
  write_graph(y_int_g, paste0(name,".graphml"), format="graphml")
  write_graph(y_int_g, paste0(name,".txt"), format="edgelist")
  print(paste("Assortativity (by deg):",assortativity_degree(y_int_g)))
  print(paste("Transitivity (clustering coeff):",transitivity(y_int_g)))
  mytable[as.character(years[i]),] <- c(vcount(y_int_g),ecount(y_int_g),
                                        max(degree(y_int_g)),length(V(y_int_g)[V(y_int_g)$deg==0]),
                                        assortativity_degree(y_int_g), transitivity(y_int_g))
  print("*****")
}

print(mytable)

communities_mat <- communities_mat[order(communities_mat[,"1971"],decreasing=F),]
print(head(communities_mat))

compare_comm_mat <- matrix(NA,nrow = num_common_nodes, ncol=num_common_nodes)
colnames(compare_comm_mat) <- rownames(compare_comm_mat) <- rownames(communities_mat)
head(compare_comm_mat)

#check how many times each person appear in the same community
for(i in 1:num_common_nodes){
  for(j in 1:num_common_nodes){
    if(j>=i){
      compare_comm_mat[i,j]<-sum(communities_mat[i,]==communities_mat[j,])
    }
    else{
      compare_comm_mat[i,j]<-compare_comm_mat[j,i]
    }
  }
}



mypal=c("white",rev(viridis(256,option="inferno")))

#ccm <- melt(compare_comm_mat)
#names(ccm) <- c("person", "person", "commcomm")

plot_ccm <- plot_ly(x=rownames(communities_mat), 
                    y =rownames(communities_mat),  
                    z=compare_comm_mat,
                    type = 'heatmap',
                    colors = mypal)%>%
  layout(title = "common communities")
show(plot_ccm)
######

#build graph with temporal communities
palette <- brewer.set1(n=3)
g_comm <- graph_from_adjacency_matrix(compare_comm_mat, 
                                      weighted = TRUE,
                                      mode="upper",
                                      add.rownames = "name")

g_comm <- simplify(g_comm, remove.loops = TRUE)
V(g_comm)$community <- membership(cluster_louvain (g_comm))
#palette <- c("red","blue","gold"),
V(g_comm)$color <- palette[V(g_comm)$community]
E(g_comm)$community <- 0 
E(g_comm)$color <- "light grey"
for(ei in 1:ecount(g_comm)){
  e <- E(g_comm)[ei]
  from <- ends(g_comm,e)[1]
  to <- ends(g_comm,e)[2]
  if(V(g_comm)[from]$community == V(g_comm)[to]$community){
    E(g_comm)[e]$community=V(g_comm)[from]$community
    E(g_comm)[e]$color=V(g_comm)[from]$color
  }
}

palette <- brewer.set1(n=3)
V(g_comm)$deg <- degree(g_comm)

pdf(file=paste0(path,"experiments/plots/intersection/poster/TemporalCommunitiesNET.pdf"))
plot(g_comm, 
     #layout=layout_base,
     layout=layout_with_fr(g_comm,dim=2,
                                 niter=50000, 
                                  grid="nogrid", 
                                 weights=E(g_comm)$weight),
     vertex.size= 5,
     #vertex.shape=ifelse(V(y_int_g)$bridge, "csquare", "circle"),
     #edge.arrow.size=0.001,
     vertex.label.cex = 0.5,
     vertex.label.color = adjustcolor("black", alpha.f = 1),
     vertex.frame.color = adjustcolor("black", alpha.f = 0.3),
     vertex.color = adjustcolor(V(g_comm)$color, alpha.f = 0.25),
     vertex.label.dist=0.3, 
     vertex.label.family="Helvetica",
     edge.width = 0.5*E(g_comm)$weight,
     #edge.curved=0.3, 
     edge.color= adjustcolor(E(g_comm)$color, alpha.f = 0.35),
     vertex.label=V(g_comm)$name)
     #main="Temporal Communities")
dev.off()
####################
####################
####################


ranked_by_deg_nodes <- head(V(gc)[order(V(gc)$deg, decreasing = T)]$name, 51)
intersect(ranked_by_deg_nodes,common_nodes)


# print(V(g)[unlist(selected)])
# 
# btw_mat <- matrix(0,nrow = length(unlist(selected)),ncol=length(years))
# colnames(btw_mat) <- years
# rownames(btw_mat) <- V(g)[unlist(selected)]$name
# 
# for(p in V(g)[unlist(selected)]$name){
#   print(p)
#   for(i in 2:length(years)){
#     yg <- intersectGlist[[i]]
#     year <- years[i]
#     print(year)
#     print(p %in% V(yg)$name)
#     btw_val <- V(yg)[V(yg)$name==p]$btw
#     btw_mat[p,as.character(year)] <- btw_val
#   }
#   print("****")
# }
# 
# 
# neighbors(g,V(g)["Gehart Friedrich"])
# #image(bxt_mat)
# 
# 
# mypal=rev(viridis(256,option="inferno"))
# 
# bxt <- melt(btw_mat)
# names(btw) <- c("people", "year", "btw_c")
# 
# plot_bxt <- plot_ly(x=sorted_books, y =dates,  z=bxt_mat,
#                     type = 'heatmap',
#                     colors = mypal)%>%
#   layout(title = "citations")
# show(plot_bxt)
# p

V(gc)$color="grey"
V(gc)$community=0
V(gc)[common_nodes]$color=V(g_comm)[common_nodes]$color
V(gc)[common_nodes]$community=V(g_comm)[common_nodes]$community

V(gc)$vip=0
V(gc)[common_nodes]$vip=1

gc <- as.undirected(gc)
E(gc)$w=1
gcs <- igraph::simplify(gc, remove.multiple = TRUE, 
               edge.attr.comb = list(w=function(x) sum(x),
                                     date="ignore",words="ignore",year="ignore"))

vip_edges <- E(g_comm)

E(gcs)$community <- 0
E(gcs)$color <- "light grey"
for(ei in 1:ecount(gcs)){
  e <- E(gcs)[ei]
  from <- ends(gcs,e)[1]
  to <- ends(gcs,e)[2]
  if(V(gcs)[from]$community == V(gcs)[to]$community && V(gcs)[from]$community!=0){
    E(gcs)[e]$community=V(gcs)[from]$community
    E(gcs)[e]$color=V(gcs)[from]$color
  }
}


V(gcs)$community <- membership(cluster_louvain (gcs))
palette <- viridis(40)
V(gcs)$color  <- palette[V(gcs)$community]
V(gcs)$deg=degree(gcs)

summary(gcs)
plot(gcs, 
     layout=layout_with_fr(gcs,dim=2,niter=50,grid="nogrid"),
     vertex.size= ifelse(V(gcs)$vip, 1, 0.2),
     #vertex.shape=ifelse(V(y_int_g)$bridge, "csquare", "circle"),
     edge.arrow.size=0.001,
     vertex.label.cex = 0.7,
     vertex.label.color = adjustcolor("black", alpha.f = 1),
     vertex.frame.color=NA,
     vertex.color = adjustcolor(V(gcs)$color, alpha.f = 1),
     #vertex.label.dist=0.3, 
     edge.width = ifelse(E(gcs)$community==0,0.05,0.8),
     #edge.curved=0.3, 
     edge.color= adjustcolor(E(gcs)$color, alpha.f = 0.5),
     vertex.label=NA,#ifelse(V(g)$vip,V(g)$name,NA),
     main="Complete Graph")

ego_net <- induced_subgraph(gcs,ego(gcs,order=1,nodes=1)[[1]])
plot(ego_net, 
     layout=layout_with_fr(ego_net,dim=2,niter=50,grid="nogrid"),
     vertex.size= 1,
     #vertex.shape=ifelse(V(y_int_g)$bridge, "csquare", "circle"),
     #edge.arrow.size=0.001,
     vertex.label.cex = 0.7,
     vertex.label.color = adjustcolor("black", alpha.f = 1),
     vertex.frame.color=NA,
     vertex.color = adjustcolor("red", alpha.f = 0.5),
     #vertex.label.dist=0.3, 
     edge.width = 0.05,
     #edge.curved=0.3, 
     edge.color= adjustcolor("light grey", alpha.f = 0.8),
     vertex.label=NA,#ifelse(V(g)$vip,V(g)$name,NA),
     main="EgoNet Bruno Kreisky")
