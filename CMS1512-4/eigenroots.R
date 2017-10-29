# Name:Atish Dixit
# Roll No.: CMS1512
# Assignment No.: 3 (with different approach)
# Date created/ modified: 11-10-15 / 12-10-15
# Comment: The coefficients were created randomly while plotting, hence this code doesn't quite match what was stated in the problem statement.

eigenroot <- function(z)
{
n<-length(z)
a<-matrix(0,n-1,n-1)
for(i in 1:n-1)
	{
		a[1,i]=-(z[n-i]/z[n])
	}
a[row(a)==col(a)+1]=1
return(eigen(a)$values)
}

degree<- 10
density<- 10000
xr<- NULL
xi<- NULL
for (i in 1:density)
	{
	ans <-eigenroot(replicate(sample(2:degree+1,1),sample(c(-1,1),1)))
	xr <- c(xr, Re(ans))
	xi <- c(xi, Im(ans))
	}

plot(xr,xi,pch=".",asp=1,yaxt='n',xaxt='n',frame.plot=FALSE, xlab='', ylab='', xlim=c(-2, 2 ), ylim=c(-2,2),main = "Random Doughnut of 10th Degree")
lines( 2*cos(seq(0,2*pi,(pi/30))) , 2*sin(seq(0,2*pi,(pi/30))) )
