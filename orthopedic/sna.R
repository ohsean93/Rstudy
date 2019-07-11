#소셜 네크워크 분석

install.packages("sna")
library(sna)

amatrix <- rbind(c(0,1,1,1,0),c(1,0,0,1,0),c(1,0,0,0,0),c(1,1,0,0,1),c(0,0,0,1,0))
amatrix

gden(amatrix)
degree(amatrix)
closeness(amatrix)
betweenness(amatrix)

gplot(amatrix)
gplot(amatrix, mode = "circle")
plot.sociomatrix(amatrix)
