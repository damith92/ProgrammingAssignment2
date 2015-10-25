## Put comments here that give an overall description of what your
## functions do
#
# Below are two functions that are used to create a special object that stores 
# a matrix and caches its inverse. In these functions we will take advantage of 
# the scoping rules of the R language and manipulate them to preserve state   
# inside of an R object to cache the inverse of a matrix.
#
## Write a short comment describing this function
#
# The first function, makeCacheMatrix creates a special "matrix", which is really 
# a list containing a function to
#
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of inverse of the matrix
# 4.get the value of inverse of the matrix
#
#
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
#
## Write a short comment describing this function
#
# The cacheSolve function calculates the inverse of the special "matrix" created 
# with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the matrix and sets 
# the value of the inverse in the cache via the setinverse function.
#
#
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)){
        message("Getting cached data.")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data)
    
    x$setinverse(inv)
    
    inv
    ## Return a matrix that is the inverse of 'x'
}
