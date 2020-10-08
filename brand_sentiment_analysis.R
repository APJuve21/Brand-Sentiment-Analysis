#install.packages("psych")
#install.packages("GPArotation")
library(psych)
library(GPArotation)
library(reshape)
library(ggplot2)

load("brand.RData")

#cor() correlation matrix
cor(x=brand.ratings[,1:9])

#principal() principal component factor analysis
#r= correlation matrix or raw data
#nfactors= number of factors
#rotate="none" no rotation
#covar=T to use covariance matrix
fit<-principal(r=brand.ratings[,1:9], nfactors=9,rotate="none")
fit
#print(fit, digits=3, cutoff=.3, sort=TRUE)

#scree() scree plot
#rx= correlation matrix or raw data
#factor=FALSE do not draw scree for factors
scree(rx=brand.ratings[,1:9],factor=FALSE)

#3 factors
fit <- principal(r=brand.ratings[,1:9], nfactors=3,rotate="none")
fit

fit$loadings
fit$communality
fit$uniquenesses

#rotate="varimax" varimax rotation
#method="regression" factor scores estimated by regression method
fitr <- principal(r=brand.ratings[,1:9],nfactors=3,rotate="varimax",method="regression")
fitr

#rotation matrix (permutated)
fitr$rot.mat[c(1,2,3),c(2,1,3)]

fit$loadings %*% fitr$rot.mat[c(1,2,3),c(2,1,3)]
fitr$loadings

#standardized values of x1-x9
head(scale(brand.ratings[,1:9]))
#coefficients to calcualte the factor scores
fitr$weights
#factor scores
head(fitr$scores)


scores<-cbind(brand.ratings["brand"],fitr$scores)

#group means of factor scores
tb<-aggregate(.~brand,data=scores,FUN=mean)
tb

#plot of the group means
tbm<-melt(tb,id.vars='brand')
ggplot(tbm, 
       aes(x = variable, y = value, group = brand, colour = brand)) + 
  geom_line(aes(linetype=brand))+
  geom_point(aes(shape=brand)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean") +
  scale_shape_manual(values=1:nlevels(tbm$brand)) +
  scale_color_manual(values=c(1,2,2,3,4,5,5,3,3,1))


#2-factor solution
fit <- principal(r=brand.ratings[1:9], nfactors=2,rotate="varimax")

#plot of factor loadings
ld<-data.frame(fit$loadings[,1:2])
ggplot(data=ld,aes(x=RC1,y=RC2))+
  geom_point()+
  geom_text(aes(label=rownames(ld),vjust=1))+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))+
  coord_cartesian(xlim = c(-1,1),ylim=c(-1,1)) 


#means factor scores
sc<-data.frame(fit$scores,brand=brand.ratings$brand)
tb<-aggregate(.~brand,data=sc,FUN=mean)
#plot
ggplot(data=tb,aes(x=RC1,y=RC2))+
  geom_point()+
  geom_text(aes(label=brand,vjust=1))+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))


#5-cluster solution by Ward's method
dist<-dist(sc[1:2],method="euclidean")^2
fitc <- hclust(dist, method="ward.D")
sol <- cutree(fitc, k=5)
sol<-data.frame(sol,sc)
sol$sol<-factor(sol$sol)
ggplot(data=sol,aes(x=RC1,y=RC2,colour=sol))+
  geom_point(aes(shape=sol))+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))



