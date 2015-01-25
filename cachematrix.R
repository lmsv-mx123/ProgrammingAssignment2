## Matrix inversion is usually a costly computation and having to calculate it repeatedly reduces a program's performance.

## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Essentialy, it is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The second function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## For a given square and invertible matrix, it attempts to retreive (from the cache) its inverse and if it exists, returns the value directly without recalculating it.
## Otherwise, the function makes use of the function solve to compute the inverse of the matrix and gives out the result.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

## Sample run:
## > x <- stats::rnorm(16)
## > dim(x) <- c(4,4)
## > mat = makeCacheMatrix(x)
## > mat$get()
##          [,1]       [,2]        [,3]       [,4]
## [1,]  1.0018393 -0.2972611  2.22514259  1.3585897
## [2,]  0.7032747 -0.9090171  0.83126950  0.5856698
## [3,] -1.0189279 -1.2232533 -1.55259378 -0.6937606
## [4,] -0.5941141 -0.8620277  0.01385461 -0.4573768
## > system.time(invMat <- cacheSolve(mat))
## user  system elapsed 
## 0.00    0.00    1.08 
## > invMat
##          [,1]       [,2]       [,3]       [,4]
## [1,] -1.30859094  1.8201337 -0.9026021 -0.1872659
## [2,]  0.08049798 -0.6202601 -0.2187170 -0.2233753
## [3,]  0.10224005 -0.1694342 -0.5796517  0.9659637
## [4,]  1.55118793 -1.2003958  1.5671058 -1.4928696
## > system.time(invMat <- cacheSolve(mat))
## getting cached data
## user  system elapsed 
## 0       0       0 
## > invMat
##          [,1]       [,2]       [,3]       [,4]
## [1,] -1.30859094  1.8201337 -0.9026021 -0.1872659
## [2,]  0.08049798 -0.6202601 -0.2187170 -0.2233753
## [3,]  0.10224005 -0.1694342 -0.5796517  0.9659637
## [4,]  1.55118793 -1.2003958  1.5671058 -1.4928696