# Name:Atish Dixit
# Roll No.: CMS1512
# Assignment No.: 5
# Date created/ modified: 22-10-15 / 23-10-15

#numerical method to find square root usinf Ancient Indian Method
indian_root <- function(Number, initial_value = Number/2 , values)
{
iterations <- 500
epsilon <- 1e-8
x <- initial_value

for (i in 2:iterations)
	{
	an <- ( Number - x[i-1]^2 ) / (2*x[i-1])
	x <- c(x, x[i-1] + an + ( 0.5 * (an^2 / (an + x[i-1])) ) )
	if (abs(x[i-1] - x[i]) < epsilon )
		{
		if( missing(values) )	return(x[i])
		else			return(x)
		}
	}

if( missing(values) )	return(x[i])
else			return(x)
}

#numerical method to find square root usinf Babylonian Method
baby_root <- function(Number, initial_value = Number/2, values)
{
iterations <- 500
epsilon <- 1e-8		
x <- initial_value

for (i in 2:iterations)
	{
	x <- c(x, 0.5*(x[i-1] + (Number / x[i-1])) )
	if (abs(x[i-1] - x[i]) < epsilon )
		{
		if( missing(values) )	return(x[i])
		else			return(x)
		}
	}

if( missing(values) )	return(x[i])
else			return(x)
}

#comparison of both the methods
pdf ('comparison.pdf')
 
 op <- par(mfrow = c(1,1))
 plot(indian_root(100,500,values),type = "b",ylim = c(0,500), col = "red",main = "Comparison of Babylonian and Ancient Indian Method \n Square root of 100",ylab = "square root values", xlab = "iteration number")
 abline(h=10,col = "gray")
 abline(v=12, col = "blue")
 abline(v=19, col = "red")
 lines(baby_root(100,500,values),type = "b", col = "blue")
 legend('topright',c("Ancient Indian Method", "Babylonian Method"),pch = "o",col=c("red","blue"), bg="white")

 x <- seq(10,300,10)
 ind_iter <- NULL
 baby_iter <- NULL
 for (i in x)
	{
	ind_iter <- c(ind_iter, length( indian_root(i,i/2,values)))
	baby_iter <- c(baby_iter, length( baby_root(i,i/2,values)))
	}
 plot(x,ind_iter,type= "b", col = "red", ylim = c(0,15), main = "Computational Comparison between \n Ancient Indian Method and Babylonian Method", xlab = "Numbers for calculating the square root", ylab = "Number of iterations for calculations")
 title(sub = "*Initial guess = Number / 2", col = "light gray", cex = 0.5)
 lines(x,baby_iter,type= "b", col = "blue")
 legend('topright',c("Ancient Indian Method", "Babylonian Method"),pch = "o",col=c("red","blue"), bg="white")

dev.off()

pdf('methods_analysis.pdf')

 
 #Analysis of Babylonian Method
 op <- par(pch = 20 , mfrow = c(1,1))
 plot(baby_root(100,100,values),type = "b",ylim = c(-100,100), col = "red",main = "Analysis of Babylonian Method \n Square root of 100",ylab = "square root values", xlab = "iteration number" )
 abline(h=10,col = "gray")
 abline(h=-10,col = "gray")
 lines(baby_root(100,50,values),type = "b", col = "blue")
 lines(baby_root(100,30,values),type = "b", col = "maroon")
 lines(baby_root(100,-30,values),type = "b", col = "green")
 lines(baby_root(100,-50,values),type = "b", col = "black")
 lines(baby_root(100,-100,values),type = "b", col = "orange")
 legend('topright',c("initial guess = -100", "initial guess = -50","inital guess = -30","initial guess = 30","initial guess = 50","initial guess = 100"),pch=20,col=c("red","blue","maroon","green","black","orange"), bg="white",ncol = 2,cex = 0.75)

 #Analysis of Ancient Indian Method
 op <- par(pch = 20 )
 plot(baby_root(100,100,values),type = "b",ylim = c(-100,100), col = "red",main = "Analysis of Ancient Indian Method \n Square root of 100",ylab = "square root values", xlab = "iteration number")
 abline(h=10,col = "gray")
 abline(h=-10,col = "gray")
 lines(indian_root(100,50,values),type = "b", col = "blue")
 lines(indian_root(100,30,values),type = "b", col = "maroon")
 lines(indian_root(100,-30,values),type = "b", col = "green")
 lines(indian_root(100,-50,values),type = "b", col = "black")
 lines(indian_root(100,-100,values),type = "b", col = "orange")
 legend('topright',c("Initial guess = -100", "Initial guess = -50","Inital guess = -30","Initial guess = 30","Initial guess = 50","Initial guess = 100"),pch=20,col=c("red","blue","maroon","green","black","orange"), bg="white",ncol = 2, cex = 0.75)

dev.off()


