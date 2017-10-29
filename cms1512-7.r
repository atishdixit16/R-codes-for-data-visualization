int.to.digit <- function (num, base)
{
digits <- NULL
while (num != 0)
	{
	digits <- c(digits, num %% base)
	num <- as.integer(num / base)
	}
return(digits)
}

digit.to.int <- function (digits, base)
{
num <- 0
for (i in 1:length(digits))
	num <- num + digits[i]*base^(i-1)
return(num)
}

kaprekar.step <- function (num, base, digit_count)
{
digits <- sort ( int.to.digit(num,base)  )
if (length(digits) < digit_count)
	digits <- c(digits, seq(0,by = 0,length.out = digit_count - length(digits)) )
digits <- sort(digits)
return ( abs(  digit.to.int(rev(digits),base) - digit.to.int(digits,base) )  )
}

kaprekar.routine <- function (num, base, digit_count)
{
if (num == 0)
	return(num)
series <- NULL
repeat
	{
	series <- c(series,num)
	num <- kaprekar.step(num, base, digit_count)
	if (num == 0)
		return(c(series,num))
	if ( any(series == num))
		break
	}
return(series)
}

base <- 10
digit_count <- 4
matrix_length <- sqrt(base^digit_count)
kaprekar_matrix <- matrix(0,matrix_length,matrix_length)
for (i in 0:matrix_length-1 )
	for (j in 0:matrix_length-1)
		kaprekar_matrix[i+1,j+1] = length ( kaprekar.routine(i*matrix_length+j,base,digit_count) ) - 1
image(0:99,0:99,kaprekar_matrix, col = terrain.colors(7), axes = F,main = 'Kaprekar Routine \n Base = 10  Digits = 4',sub = '*colored by number of iterations',xlab = 'n div  100 ',ylab = 'n mod  100 ' )
axis(2,at = c(0,99))
axis(1,at = c(0,99))

cyclic_pattern <- function(k)
{
initial_vector <- kaprekar.routine(1, 10, k)
for (i in 1:(10^k-1))
{
	if ( length( unique(int.to.digit(i,10))) != 1) 
	{
		initial_vector <- intersect(initial_vector, kaprekar.routine(i,10,k) )
	}
}
return(initial_vector)
}

cat('Kaprekar cyclic pattern for base=10 and digit=2 : ','{0}','{',cyclic_pattern(2),'}','\n' )
cat('Kaprekar cyclic pattern for base=10 and digit=3 : ','{0}','{',cyclic_pattern(3),'}','\n' )
cat('Kaprekar cyclic pattern for base=10 and digit=4 : ','{0}','{',cyclic_pattern(4),'}','\n' )
