## The 2 functions provide functionality to cache the inverse of
## a given matrix and reuse it later. Calculating the inverse is
## only done if it hasn't been done before for the given martix.

## Usage:   Let A be an invertable matrix
##          > m <- makeCacheMatrix(A)       ## Creates a matrix object.
##          > cacheSolve(m)                 ## Retrieves the inverse of
                                            ## the matrix in m.



## The function makeCacheMatrix returns a list of functions
## to set a matrix and its inverse as well as accessing them.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           ## The inverse matrix initializes to NULL
  
    set <- function(y) {
        x <<- y
        inv <<- NULL      ## The inverse matrix in set to NULL when 
    }                     ## a new matrix is set

  
    get <- function() x
  
    setinv <- function(x_inverse) inv <<- x_inverse
  
    getinv <- function() inv
  
    list(set = set, get = get,    ## makeCacheMatrix returns a list
         setinv = setinv,         ## of the 4 functions defined.
         getinv = getinv)
}



## The function cacheSolve returns the inverse of a given matrix
## that was created using the makeCacheMatrix function. The function
## returns the cached version of the matrix if it exists. Otherwise
## it calculates the inverse, adds it to the cache and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
