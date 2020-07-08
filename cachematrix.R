## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a new class, which is able to cache it's inverse.
## Author: Mark Renslow July 8, 2020
makeCacheMatrix <- function(x = matrix()) {
  
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        set_inv <- function(solve) x_inv <<- solve
        get_inv <- function() x_inv
        
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
        

}


## Write a short comment describing this function
## this function works like solve, but it caches the result for retrieval
## later.
## Author: Mark Renslow
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        x_inv <- x$get_inv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        
        temp <- x$get()
        x_inv <- solve(temp, ...)
        x$set_inv(x_inv)
        x_inv
}
