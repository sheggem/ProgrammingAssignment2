
## These two functions will calculate the inverse matrix + cache the result
## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setCache <- function(inversematrix) inverse <<- inversematrix
  getCache <- function() inverse
  list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', use cache if available  
  inverse <- x$getCache()
  
  ## Check if matrix has already been cached
  if(!is.null(inverse)) {
    message("Getting cached matrix.")
    return(inverse)
  }
  
  ## Get matrix and solve + set cache
  data <- x$get()
  inverse <- solve(data)
  x$setCache(inverse)
  
  ## Return inversed matrix
  inverse  
}


##Code to test 
##http://www.mathwords.com/i/inverse_of_a_matrix.htm
testMatrix <- matrix(c(1,3,2,4),2,2)
testMatrix
# [,1] [,2]
# [1,]    1    2
# [2,]    3    4
##Plain solve
solve(testMatrix)

##Using custom fuctions
m = makeCacheMatrix(testMatrix)
m$get()
cacheSolve(m)
cacheSolve(m)
# 
# Getting cached matrix.
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
