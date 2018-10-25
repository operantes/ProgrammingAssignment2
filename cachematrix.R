## Put comments here that give an overall description of what your
## functions do

##################################################
## 
## FUNCTION makeCacheMatrix(
##				x - the input matrix whose cached inverse is to be created.
##		)
##
##		Creates a hidden matrix meant to store the inverse matrix of the one referenced
##			as a formal parameter. The function just creates the space for the cached inverse.
## 			No output provided. 
##		This function acts in a similar way constructors do, in the context of Object Oriented languages.
##
##		$Version$ 
## 
##################################################
makeCacheMatrix <- function( x = matrix() ) {
	## How should the inverse matrix we modeled?
	## 		- As an attribute of the matrix object?
	##			Attributes are stored as a set, not as a matrix. Lack of ordering in between values might become a hindrance further on.
	##		- Should the input matrix be converted into the first element of a vector, or a list, of 2 matrices?
	
	## Should reservation for the inverse matrix do any prechecks on the input matrix?
	
	## The following variable lays cached (hidden) within the function. It is only acccessed through
	## the following set of functions.
	##MyCachedInverse <<- matrix( data = NA, nrow = dim(x)[2], ncol = dim(x)[1], byrow = FALSE, dimnames = NULL )
	MyCachedInverse <- NULL
  
	## The following functions act as public methods. 
	set <- function( y ) {
		x <<- y
		MyCachedInverse <<- NULL
	}
	get <- function() x
	setInverse <- function( MyInverse ) MyCachedInverse <<- MyInverse
	getInverse <- function() MyCachedInverse
	
	## 
	list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}

##################################################
## 
## FUNCTION cacheSolve(
##				x - the input matrix whose inverse is to be calculated.
##		)
##
##		Calculates the inverse of the referenced matrix.
##		- The function 
##			1) first assumes that the formal parameter has an environment that includes the so-called
##			  	public methods defined by the makeCacheMatrix() function.
##			2) then invokes the referenced public methods, either for reading, or first creating and
##				then reading the inverse matrix.
##
##		$Version$ 
## 
##################################################
cacheSolve <- function( x, ... ) {
    ## Return a matrix that is the inverse of 'x'

	## Confirm that it is a square matrix by comparing whether all dimensions are identical to the first one.
	MyAuxDimension <- rep( dim( x )[1], length( dim( x ) ) )
	if ( identical( dim( x ), MyAuxDimension ) ) {
      MyCachedInverse <- x$getInverse()
	    if( !is.null( MyCachedInverse ) ) {
	    		message("Getting cached data")
  		} else {
	    		data <- MyCachedInverse$get()
			    MyCachedInverse<- solve( data )
    			x$setInverse( MyCachedInverse )
		  }
		  return( MyCachedInverse )
	} else {
		  message( "Not a square matrix. Consider calculating the pseudoinverse, instead." )
	}		
}
