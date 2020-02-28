install.packages("ggplot2")
install.packages("ggfortify")
install.packages("digest")
#install.packages("factoextra")
library(ggplot2)
library(digest)
library(ggfortify)
#library(factoextra)


data1<-read.csv(file="/Users/ybdon/Desktop/Codeaton2020/Obese_Lean_Microbiome_Data_2.csv", row.names=1)  #change file directory 
data1$family<-factor(data1$family)
data2<-data1[c(1:5462)]
data2.pca<-prcomp(data2,scale=TRUE)

#pca to obesitycat
pdf("/Users/ybdon/Desktop/Codeaton2020/obesity.pdf") #change file directory
autoplot(data2.pca,data=data1,colour='obesitycat')
dev.off()

#pca to family
pdf("/Users/ybdon/Desktop/Codeaton2020/family.pdf") #change file directory
autoplot(data2.pca,data=data1,colour='family')
dev.off()

#pca to age
pdf("/Users/ybdon/Desktop/Codeaton2020/age.pdf") #change file directory
autoplot(data2.pca,data=data1,colour='age')
dev.off()

#pca to ancestry
pdf("/Users/ybdon/Desktop/Codeaton2020/ancestry.pdf") #change file directory
autoplot(data2.pca,data=data1,colour='ancestry')
dev.off()

######################### delete two outer row 155, 205

data2.deleterows<-data2[-c(155,205),]
data1.deleterows<-data1[-c(155,205),]
data2.deleterows.pca<-prcomp(data2.deleterows)

pdf("/Users/ybdon/Desktop/Codeaton2020/obesitycat_dele.pdf") #change file directory
autoplot(data2.deleterows.pca,data=data1.deleterows,colour='obesitycat')
dev.off()
pdf("/Users/ybdon/Desktop/Codeaton2020/ancestry_dele.pdf") #change file directory
autoplot(data2.deleterows.pca,data=data1.deleterows,colour='ancestry')
dev.off()
pdf("/Users/ybdon/Desktop/Codeaton2020/age_dele.pdf") #change file directory
autoplot(data2.deleterows.pca,data=data1.deleterows,colour='age')
dev.off()
pdf("/Users/ybdon/Desktop/Codeaton2020/ancestry_and_obesitycat_dele.pdf") #change file directory
autoplot(data2.deletemulrows.pca,data=data1.deletemulrows,colour='ancestry',shape='obesitycat')
dev.off()


################################### column1000-1250
data2.250col<-data2.deleterows[,c(1000:1250)]
data2.250col.pca<-prcomp(data2.250col)
autoplot(data2.250col.pca,data=data1,colour='family')
autoplot(data2.250col.pca,data=data1,colour='ancestry')
autoplot(data2.250col.pca,data=data1,colour='obesitycat')
autoplot(data2.250col.pca,data=data1,colour='age')

#######################data2.trans : transfer all values into 0 or 1 (if value >0, then change the value into 1,else 0)
###################### use this 0 and 1 data frame for phylogeny of 281(or 279) samples  
data2.trans<-data2
data2.trans[c(1:5462)][data2.trans[c(1:5462)]>0]=1
data2.trans.pca<-prcomp(data2.trans,scale=TRUE)
autoplot(data2.trans.pca,data=data1,colour='obesitycat')
autoplot(data2.trans.pca,data=data1,colour='ancestry')

write.csv(data2.trans,"/Users/ybdon/Desktop/Codeaton2020/281phy.csv")




#################data2.transpose 
data2.transpose<-t(data2)
data2.transpose.pca<-prcomp(data2.transpose,scale=TRUE)
autoplot(data2.transpose.pca)

