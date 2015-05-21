## Put comments here that give an overall description of what your
## functions do

## The first function creates a special object that can be used to store both
## a matrix and its inverse

## The second function gets the inverse of the special object, from its cache
## if it exists.

## This function create a CacheMatrix object which consists
## of a list of 4 functions, get/set the matrix and 
## get/set its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## cached inverse of the matrix
    i <- NULL
    
    ## setter function
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    ## getter function
    get <- function(){ x }
    
    ## setter for the inverse
    setInverse <- function(inverse){ i <<- inverse }
    
    ## getter for the inverse
    getInverse <- function(){ i }
    
    ## list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function take a cacheMatix object and returns a
## cached inverse, if it does not exist it will solve
## it, store it, then return it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## attempt to retrieve inverse from cache
    i <- x$getInverse()
    
    ##check if anything was returned
    if(!is.null(i)){        
        message("getting cached data")
        ## if so return the data from cache
        return (i)
    }
    
    ## if not get the original matrix
    data <- x$get()
    
    ## solve its inverse
    i <- solve(data, ...)
    
    ## store in the cache
    x$setInverse(i)
    
    ## return the calculated inverse
    i
}
