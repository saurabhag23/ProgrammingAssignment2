## This file contains two functions that work together to cache the inverse
## of a matrix, avoiding repeated expensive computations.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # Function to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset the cached inverse when matrix changes
        }
        
        # Function to get the matrix
        get <- function() x
        
        # Function to set the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # Function to get the inverse
        getinverse <- function() inv
        
        # Return a list of functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Try to get the cached inverse
        inv <- x$getinverse()
        
        # If the inverse is already cached, return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, get the matrix
        data <- x$get()
        
        # Compute the inverse using solve()
        inv <- solve(data, ...)
        
        # Cache the inverse
        x$setinverse(inv)
        
        # Return the inverse
        inv
}


## Example usage:
## > m <- matrix(c(4, 3, 3, 2), 2, 2)
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)  # First call computes the inverse
## > cacheSolve(cm)  # Second call retrieves from cache with message