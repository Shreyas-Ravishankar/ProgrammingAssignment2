makeCacheMatrix <- function(x = matrix()) ##The argument is defined with default mode
{
  invmat <- NULL      ##Inverse of the atrix is initially set to Null
  assign<-function(b) ##A new matrix is assigned
  {
    x<<-b			  ##The value of the matrix in the parent environment
    invmat<<-NULL	  ##The inverse is reset to null if there is a new matrix.
  }
  input <- function() ##The function used to retun the value of the argument which contains the matrix
    x
  assigninv<-function(invert) 
    invmat<<- invert  	  ##This function assigns the value of a in the parent environment
  getinv<-function()  ##Used to intake the value of invmat when called
    invmat
  list(assign = assign, input = input, assigninvert = assigninvert, getinvert = getinvert)	## Required to refer to the $function
  
  ##The above function is used to compute the inverse of a special matrix. 
  ##If the inverse of the matrix has been previously computed and it happens to be the same, then the cacheSolve will return the inverse from the cache.
  
  cacheSolve <- function(x, ...) ##Return the inverse of x
  {
    invmat <- x$getinv()
    if(!is.null(invmat))  ##to check if invmat holds null or not
    {
      message("Displaying the value of inverse from cache")
      return(invmat)
    }
    
    d <- x$input()
    invmat <- solve(d,...)	##Computing the inverse
    x$assigninv(invmat)	##Assign the inv to parent matrix
    invmat			
  }
}

  
  
  
  