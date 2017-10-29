# Name:Atish Dixit
# Roll No.: CMS1512
# Assignment No.: 3
# Date created/ modified: 11-10-15 / 12-10-15

# function for finding roots using eigenvalue approach
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

# function for converting decimal numbers in binary format
IntToBin <- function(n)
{
	bin <- NULL
	if (n==0)
	{
		return(c(bin,0))
	}
	else
	{
		while (n >= 1)
		{
			bin <- c(bin,n%%2)
			n = (n - n%%2)/2
		}
		return(rev(bin))
	}	
}

# function to create coefficents of various combination of -1 and +1 values
coeff <- function(num ,degree)
{
	x <- IntToBin(num)
	if (length(x) != degree+1)
	{
		x <- c( rep(0, (degree + 1 - length(x))) , x )
		return(replace(x, x==0, -1))
	}
	else
		return(replace(x, x==0, -1))
}

# plotting the roots at various degrees of polynomial functions
pdf("Doughnuts.pdf")

colors <- c("magenta","red","blue","maroon","purple","black")
for (degree in 5:10)
{
	degreevector <- (1:degree)
	xr <- NULL
	xi <- NULL

	for (i in degreevector)
	{	
		for (j in 0:( (2)^(i+1) - 1 ))
		{
			ans <- eigenroot(coeff (j,i))
			xr <- c(xr, Re(ans))
			xi <- c(xi, Im(ans))
		}
	}
	plot(xr,xi,pch=".",xaxt = "n",yaxt = "n",xlab = "", ylab = "", asp=1, bty="n", ylim = c(-1.9,1.9), col=colors[degree-4])
	title( main= paste(degree,"th Degree Doughnut"), col.main = colors[degree-4], cex.main=1.5)
	lines(1.8*cos(seq(-pi,pi,by=pi/24)),1.8*sin(seq(-pi,pi,by=pi/24)),col=colors[degree-4])
}

dev.off()
