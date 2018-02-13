## R Programming - Programming Assigment 2
## Data Science Specialization
##
## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## computing it repeatedly.
## Often, especially in loops, the inverse of a given matrix
## need to be calculated only once. Caching calculation result
## saves re-computing it repeatedly.

## makeCacheMatrix function creates a special "matrix" object that
## can cache its inverse.
## 
## Input: Vvariable of type matrix
## 
## Output: A list with a inverse of a given matrix
##
## Example:
### x <- matrix(1:12, nrow = 4, ncol = 3)
### c <= makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # 1. Function seting the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 2. Function getting the value of the matrix
  get <- function() x
  
  # 3. Set the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # 4. Gets the value of the inverse of the matrix
  getinverse <- function() inv
  
  # 5. Return a result as a list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the given matrix.
## If the result has been calculated before, it will return the
## cached inverse value of the matrix. Otherwise it will calculate
## the inverse value, cache it and then return it.
##
## Input: Vvariable of type matrix 
## 
## Output: An inverse of the matrix
##
## Example:
### x <- matrix(1:12, nrow = 4, ncol = 3)
### c <- makeCacheMatrix(x)
### s1 <- cacheSolve(c)
### s2 <- cacheSolve(c)
## Running s2 should display "Getting cached data." message.

cacheSolve <- function(x, ...) {
  # 1. Get an inverse of the given matrix
  inv <- x$getinverse()
  
  # 2a. Checks whether inverse is calculated (not null)
  if(!is.null(inv)) {
    message("Getting cached data.")
    
    # 2b. amd returns it
    return(inv)
  }
  
  # 3. Otherwise, get the matrix itself
  data <- x$get()
  
  # 4. Finds the inverse
  inv <- solve(data, ...)
  
  # 5. Caches the result for the future
  x$setinverse(inv)
  
  # 6. Returns the computed result
  inv
}
