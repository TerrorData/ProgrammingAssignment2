## This R Script contains two functions that demonstrate object properties in R
##
## There are two functions containted within this code
## makeCacheMatrix - makes an object from a given matrix 
## cacheSolve - call the methods of the object created to return a cached matrix
##
## Test Calling Script
## 
## source("cachematrix.R")
##
## Create a Matrix
## m <- matrix(c(3,7,5,4),nrow=2,ncol=2)
##
## Make sure it has an inverse
## solve(m)
## [,1]       [,2]
## [1,] -0.1739130  0.2173913
## [2,]  0.3043478 -0.1304348
## 
## Create a cached matrix object, call to solve for inverse and cache results 
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)
##
## [,1]       [,2]
## [1,] -0.1739130  0.2173913
## [2,]  0.3043478 -0.1304348
## 
## cacheSolve(cm)
## 
## getting cached data
## [,1]       [,2]
## [1,] -0.1739130  0.2173913
## [2,]  0.3043478 -0.1304348
##


## makeCacheMatrix < function(x = matrix())
##      This function returns on object with four methods:
##          set - initializes the value of the object
##          get - returns the value of the object
##          setinverse - caches the inverse of the matrix
##          getinverse - retiurns the cached value of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inv) {
        m <<- inv
    }
    getinverse <- function() {
        m
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve <- function(x, ...)
##      This function calls the object methods created from makeCacheMatrix:
##          get the cached matrix of the inverse
##          if cached matrix is found
##              return inverse
##          else
##              solve for inverse of matrix
##              cache results
##              return inverse
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
