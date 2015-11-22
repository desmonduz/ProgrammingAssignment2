# This function takes square, invertible matrix as input and returns a list of functions that enable the user to set, get, 
# setmean, and getmean of the matrix. By default the matrix is empty matrix. 
# The user can populate the function with the instance of the matrix by calling makeCacheMatrix() function.

#Use: 
# makeCacheMatrix(3*diag(3)) 
# OR
# mtx <- makeCacheMatrix() #mtx is the list containing functions.
# mtx$set(3*diag(3))

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInverse <- function(invrs) m <<-invrs
    getInverse <- function() m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


#This function takes x as the list of matrix functions defined in makeCacheMatrix and returns the inverse of the matrix. 
#If the inverse has never been computed before for given matrix, it computes the inverse, stores it in cache and returns the result. 
#For further calls, the result is returned from cache unless the value of the matrix has been changed using set function.

#Use:

# cacheSolve(mtx) #computes and outputs diagonal matrix with 0.333 on its diagonal.
# cacheSolve(mtx) #returns the same matrix from cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}
