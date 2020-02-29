install.packages("ggplot2")
install.packages("ggfortify")
install.packages("digest")
install.packages("phylogram")
install.packages("dendextend")

library(dendextend)
library(phylogram)
library(ggplot2)
library(digest)
library(ggfortify)

ndata2<-read.csv(file="/Users/ybdon/Desktop/Codeaton2020/ndata2.csv") #change into your file directory
ndata2.otu<-ndata2[c(1:5462)]

ndata2.pca<-prcomp(ndata2.otu)

pdf("/Users/ybdon/Desktop/Codeaton2020/figures_from_ndata2/pca.pdf") #change into your file directory
autoplot(ndata2.pca,data=ndata2,colour='obesitycat')
dev.off()

nd<-dist(as.matrix(ndata2.otu))
nhc<-hclust(nd)
nhcd<-as.dendrogram(nhc)
nhcd<-nhcd%>%color_branches(k=10)%>%set("labels_cex",0.5)

ncolors_to_use<-as.numeric(ndata2$obesitycat)
ncolors_to_use<-ncolors_to_use[order.dendrogram(nhcd)]
labels_colors(nhcd)<-ncolors_to_use

legendlevels=unique(ndata2$obesitycat)
legendfill=unique(as.numeric(ndata2$obesitycat))

pdf("/Users/ybdon/Desktop/Codeaton2020/figures_from_ndata2_v2/hcd.pdf") #change into your file directory
circlize_dendrogram(nhcd,labels=TRUE,labels_track_height=0.1,dend_track_height=0.8)
legend("topleft",legend=legendlevels,fill=legendfill)
dev.off()

write.dendrogram(nhcd,file="/Users/ybdon/Desktop/Codeaton2020/figures_from_ndata2_v2/hcd.tree") #change into your file directory


