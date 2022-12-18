library(igraph)

node <- read.table("lastfm_asia_edges.csv",sep = ",",header = T)
head(node,10)


gr<- graph_from_data_frame(node,directed = F)
gr


#연결정도 중심화
de <- centralization.degree(gr,normalized = F)
de_tmax <- centralization.degree.tmax(gr)
de$centralization
de_tmax

#근접 중심화
ne <- centralization.closeness(gr,normalized = F)
n <- vcount(gr)
ne$centralization/(n-1)
ne$theoretical_max/(n-1)

#중개 중심화
bt <- centralization.betweenness(gr,normalized = F)
bt$centralization
bt$theoretical_max

#plot사용+xy축 레이블 작성
deg<- degree(gr)
plot(deg,xlab ="사용자 번호",ylab = "팔로우수",type = 'h')

#연결정도 상위 10개 출력 + 노드이름만 출력
deg <- sort(deg,decreasing = T)
h10 <-head(deg,10)
h10_n<- names(h10)
h10
h10_n

#그래프 작성
name <- V(gr)$name[V(gr)$name==h10_n[1]]
v2=which(V(gr)$name==name)
v.set <- neighbors(gr,v=v2)
v3 <- c(v2,v.set)
gr2 <- induced_subgraph(gr,v3)
#레이블 작성여부,색 구별
V(gr2)$color <- ifelse(V(gr2)$name==name,"red","green")
V(gr2)$label <- ifelse(V(gr2)$name==name,name,NA)

#노드크기별 사이즈 조절
V(gr2)$size <- ifelse(V(gr2)$name==name,30,5)
plot(gr2)
