## Calculate the inverse of a matrix, keeping into account if it has been calculated yet.


## Generate a list to keep together a matrix and its associated inverse matrix, with 
## function elements to get data kept in cache (yet calculated) and set data for the first time

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        ## initialize matrix
        x <<- y
        ## initialize inverse
        inv <<- NULL
    }
    ## gets the matrix
    get <- function() x
    ## calculates the inverse
    setinverse <- function(solve) inv <<- solve
    ## get the inverse matrix yet calculated
    getinverse <- function() inv
    
    ## result
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
    
}


## Lets calculate the inverse of a matrix (defined as a list with the function makeCacheMatrix)
## If the inverse has been yet calculated, it shows the result. If not, calculates it and
## this inverse matrix gets included in the list, together with the origina matrix

cacheSolve <- function(x, ...) {
    ## gets the inverse matrix in cache
    inv <- x$getinverse()
    ## the element inv is not null. The inverse is in cache, so shows it and end
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    ## inv is null, so calculates the inverse and keeps it in cache
    inv <- solve(data, ...)
    x$setinverse(inv)
    ## shows the result
    inv
}
