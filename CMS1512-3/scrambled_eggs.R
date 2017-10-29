# Name: Atish Dixit
# Roll No.: CMS1512
# Date Created/Modified: 22-09-15/23-09-15 

library(scatterplot3d)

N=500
y1<-rexp(N)
y2<-rexp(2*N)

# Demonstration of exponential distribution
pdf("air-density-histrogram.pdf")
 p<-par(mfrow=c(2,1))
 hist(y1,probability=T,ylim=c(0,1),main="Histrogram for e molecules",xlab="Distance from ground")
 lines(x<-seq(0,10,0.5),y<-exp(-x),col="red",lty=2)
 legend('topright',c("Exponential Distribution Curve"), col="red",bg="white",lty=2)
 hist(y2,probability=T,ylim=c(0,1),main="Histrogram for g molecules",xlab="Distance from ground")
 lines(x<-seq(0,10,0.5),y<-exp(-x),col="red",lty=2)
 legend('topright',c("Exponential Distribution Curve"), col="red",bg="white",lty=2)
 par(p)
dev.off()

# Plot 2-d graph
pdf("atmosphere-snapshot.pdf")
 plot(runif(N),y1,pch=20, main="2D Snapshot of Atmosphere", xlab="Ground", ylab="Elevation",xaxt='n',yaxt='n')
 points(runif(2*N),y2,col=3,pch=20)
 legend('topright',c("e - molecules", "g- molecules"),pch=20,col=c("black","green"), bg="white")
 
#plot 3-d graph
 a<-scatterplot3d(runif(N),runif(N),y1,main="3D Snapshot of Atmosphere",xlab="Ground", ylab="",zlab="Elevation",tick.marks=F,pch=20,col.axis="grey")
 a$points3d(runif(2*N),runif(2*N),y2,pch=20,col=3)
 legend('topright',c("e - molecules", "g- molecules"),pch=20,col=c("black","green"), bg="white")
dev.off()

