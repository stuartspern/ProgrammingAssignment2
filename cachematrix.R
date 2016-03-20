# MakeCacheMatrix takes an invertible matrix and embeds it in a list 
# that includes functions for>>
# set/get/set inverse/get inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # initialize the matrix inverse
    # set the components of the matrix
	set <- function(y) {
        x <<- y
        inv <<- NULL # initialization of inverse
    }
	# get the components of the matrix
    get <- function() x
	# set the components of inverse of the matrix
	# assumption is that matrix is invertible
    setinv <- function(inver) inv <<- inver
	# get the components of inverse of the matrix
    getinv <- function() inv
	#attach the inner functions as list to output of the wrapper function
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# cacheSolve returns an inverted matrix for a given input matrix
# embedded in the list object 
cacheSolve <- function(x, ...) {
	#retrieve (if available) the inverted matrix
    inv <- x$getinv() 
	# if inverted matrix exists, just return it
    if(!is.null(inv)) { 
        message("getting cached data.")
        return(inv)
    }
	# otherwise calculate the inverse of the matrix, cache it, and return inverse value
    matr <- x$get()  # get matrix from input objext
    inv <- solve(matr) # calculate inverse of matrix
    x$setinv(inv)  # cache the inverted matrix
    inv # return the inverted matrix
}
