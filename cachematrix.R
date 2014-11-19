## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special "matrix" which contains
## function to cache the inverse of a matrix
## cacheSolve returns an inverse of a matrix by first
## checking makeCacheMatrix to see if the inverse has been calculated

## Write a short comment describing this function
## this function creates a special "matrix", which is really
## a matrix containing a function to
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse 
## 4. get the value of the inverse 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    matrix(list(set, get, setinverse, getinverse), 
        dimnames=list(c("set", "get", "setinverse", "getinverse")))
}


## Write a short comment describing this function
## this function returns an inverse of a matrix by first
## checking if the matrix returned by makeCacheMatrix has been cached
## if not, it will calculate the inverse.  Otherwise, it will return
## the cache value of the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x[["getinverse",1]]()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x[["get",1]]()
    i <- solve(data)
    x[["setinverse",1]](i)
    i
}
