## Erdem John Balikci, February 8, 2017

## Programming Assignment 2: Lexical Scoping

## Matrix inversion by pair of functions 
## (makeCacheMatrix and cacheSolve) that 
## cache the invers of a matrix (invertible square)

## makeCacheMatrix is a function which creates a 
## special "matrix" object that can 
## cache its inverse for the input 
## (which is an invertible square matrix)

## set the value of matrix
## get the value of matrix
## set the inverse of matrix
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed) then the cacheSolve should retrieve the 
## inverse from the cache. 


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting the cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


## Example / Usage: 

## > m <- matrix(c(7,0,-3,2,3,4,1,-1,-2), nrow=3, ncol=3)
##> m
## [,1] [,2] [,3]
## [1,]    7    2    1
## [2,]    0    3   -1
## [3,]   -3    4   -2
## > MyMatrix <- makeCacheMatrix(m)
## > cacheSolve(MyMatrix)
## [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21