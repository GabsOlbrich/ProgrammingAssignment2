## This function first creates a way of getting the cached version of the inverse of a matrix if it has already
## been calculated, otherwise it just returns the inverse of the matrix and stores it in an object in the parent
## environment.

## makeCacheMatrix is a series of nested functions that resets any past calculations, and sets up the framework within
## which cacheSolve has to work

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){                          ## this function clears any previous values inputed into the function
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve    ## solves for the inverse of the matrix and assigns it to object m
     ## in the parent environ
     getInverse <- function() m                   ## retrieves the inverse of the initial matrix cached into m
     list(set = set, get = get, 
          setInverse = setInverse,
          getInverse = getInverse)                ##returns a list of the various functions needed to be called in cacheSolve
}


## This functions takes the object created in makeCacheMatrix and then first checks whether there is a version of the inverse of the matrix 
## stored in the cache and if so returns that, otherwise it calculates it, puts the now calculated answer into the cache and then returns the inverse
## of the original matrix inputted into makeCacheMatrix

cacheSolve <- function(x, ...) {
     m <- x$getInverse()                     ## Retrieves what is stored in the cache
     if(!is.null(m)) {                       ## if there is something stored in the cache then it returns that object 
          message("getting cached data")
          return(m)
     }
     data <- x$get()                         
     m <- solve(data, ...)                   ## retrieves the matrix inputed into makeCacheMatrix and calculates the inverse
     x$setInverse(m)                         ## stores the now calculated matrix into the cache
     m                                       ## returns the newly calculated inverse of the matrix
}