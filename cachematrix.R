## Functions for returning inverse of Matrix either from cache or computed. 
## To use instantiate makeCacheMatrix object by assigning function to variable and passing in a Matrix.
## Run cacheSolve passing in makeCacheMatrix object to return inverse of matrix.
## First run will compute inverse of matrix and store in cache.
## Subsequent runs will pull from "cache".

## Function that acts as a makeCacheMatrix object. 
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL                             ## Instantiates m "cache" variable to store inverse as NULL 
    set <- function(y) {                ## function for storing the passed in matrix
        x <<- y                           ## puts the new matrix into x
        m <<- NULL                        ## new matrix so clear any cached inverse
    }
    get <- function() x                 ## returns the matrix
    setinverse <- function(inverse) m <<- inverse  ## puts the inverse of the matrix into the m "cache" variable
    getinverse <- function() m          ## returns the inverse of matrix from "cache"
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
    # returns list of functions
}


## Function returns inversion of Matrix contained in passed instance of makeCacheMatrix object.
## Function returns cached version if it exists otherwise creates, stores in cache and then returns.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of matrix contained in passed in object
    m <- x$getinverse()                 ## runs get method on instance of makeCacheMatrix object
    if(!is.null(m)) {                   ## checks whether object contains inverted matrix
        message("getting cached data")    ## keeping results clear
        return(m)                         ## returns cached result
    }
    data <- x$get()                     ## gets the matrix to invert
    m <- solve(data)                    ## solves
    x$setinverse(m)                     ## puts the result into cache for next time
    m                                   ## returns matrix  
}