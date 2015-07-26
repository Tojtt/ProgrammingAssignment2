## Here we have two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
 

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(get = get, set = set,setinverse = setinverse, getinverse = getinverse)

}


## The CacheSolve is use to check if there is already a cached inverse the matrix x
## if there is one, then it simply returns the solution
## if not then it will calculate the inverse, store, and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
        ##assign the value of getinverse of the matrix to m
    if (!is.null(m)) {
        ## here we check if m is null (no cached inverse)
        ## if not then we return the cached value
        message("getting cache data")
        return(m)
    }
    ## if m is null then we calculate the inverse then cache it
    data <- x$get
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
