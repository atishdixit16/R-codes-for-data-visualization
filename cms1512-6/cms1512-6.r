# Name:Atish Dixit
# Roll No.: CMS1512
# Assignment No.: 6
# Date created/ modified: 01-11-15 / 03-10-15


library('lattice')

#function to calculate the GCD
gcd <- function(num1,num2,iter_count)
{
count <- 0
if (num1==0)
	{
	if(!missing(iter_count))	return(count)
	else				return(num2)
	}
if (num2==0)
	{	
	if(!missing(iter_count))	return(count)
	else				return(num1)
	}
while(num2!=0)
	{
	remainder <- num1 %% num2
	num1 <- num2
	num2 <- remainder
	count <- count + 1
	}	

if(!missing(iter_count))	return(count)
else				return(num1)
}

#function to generate the matrix
iter_matrix <- function(number_range)
{
iter_matrix <- matrix(0,number_range,number_range)
for (i in 1:number_range)
	for (j in 1:number_range)
			if (gcd(i,j) == 1)
				iter_matrix[i,j] = gcd( i, j, iter_count)
return(iter_matrix)
}

#Analysis of obtained results
pdf('cms-1512-6.pdf')

 range_max <- 100
 co_pair_matrix <- iter_matrix(range_max)	
 print( levelplot( co_pair_matrix, at = seq(1,max(co_pair_matrix), 2) , xlab = 'Number range', ylab = 'Number range', main = 'Colorplot of Co-pairs Matrix',sub='*colored by no. of iterations' ) ) 

 range <- seq(10,300,10)
 fraction <- NULL
 for (i in 1:length(range))
	{
	fraction <- c(fraction, (length( which (iter_matrix (range[i] ) != 0 ) ) + 2) / (range[i]+1)^2 )
	}

 plot(range, fraction, ylim = c(0.4,0.7), ylab = 'co-prime co-fraction value', xlab = 'max value of the number range', main = 'Co-fraction Values', sub = expression ( paste('*The gray convergence line crosses y axis at 6/',pi^{2}) )  )
 abline(h=6/pi^2, col = "gray")

dev.off()

