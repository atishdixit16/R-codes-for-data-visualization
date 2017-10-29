permutations <- function(n)
{
	if(n==1)
		return(matrix(1))
	else
	{
		sp <- permutations(n-1)
		p <- nrow(sp)
		A <- matrix(nrow=n*p,ncol=n)
		for(i in 1:n)
			A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
		return(A)
    }
}

safe <- function(p)
{
	n <- length(p)
	for (i in 1:(n-1) )
		for (j in (i+1):n)
			if ( abs(i-j) == abs(p[i] - p[j]) )
				return(FALSE)
	return(TRUE)
}

queenPermutations <- function(boardLength,quick)
{
	perms <- permutations(boardLength)
	solutions <- NULL
	for (i in 1:nrow(perms))
	{
		if (safe(perms[i,]) == TRUE)
		{
			solutions <- rbind(solutions,perms[i,])
			if (!missing(quick))
				return(solutions)
		}
	}
	return(solutions)
}

boardLength <- as.numeric(readline('Enter the size of the chessboard(> 3): '))
chessVector <- TRUE
for (i in 2:(boardLength^2) )
{
	if ( ((boardLength%%2)== 0)  &&  ((i-1)%%boardLength == 0) )
		chessVector <- c(chessVector, (chessVector[i-1]!=FALSE))
	else 
		chessVector <- c(chessVector, (chessVector[i-1]==FALSE))	
}
chessBoard <- matrix(chessVector,boardLength,boardLength)
image(1:boardLength,1:boardLength,chessBoard,xlab = '',ylab = '',main = paste( 'Quickest solution for ',boardLength,'x',boardLength,' chessboard'))
points(1:boardLength,queenPermutations(boardLength,quick)[1,],pch = 'Q')
