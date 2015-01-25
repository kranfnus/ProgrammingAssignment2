## R Programming Assignment 2
## 
## Author: Franck Formis 
## 
## cachematrix.R - Version 1.0 - 25.01.2015
## 
## This R file contains two functions makeCacheMatrix and cacheSolve to 
## cache a matrix and its inverse
## 
## The makeCacheMatrix function creates a special "matrix" object 
## which is a list of 4 functions to:
## 1) cache a matrix and its inverse through set() and setinv()
## 2) read the matrix and its inverse from the cache through get() and getinv()
## 

makeCacheMatrix <- function(x = matrix()) {
        ## When the matrix is first created, the inverse must be reset to NULL
        i <- NULL
        set <- function(y) {
                x <<- y
                ## If the matrix is changed the inverse must be reset to NULL
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## 
## The cacheSolve function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                ## If the inverse has already been calculated then
                ## `cacheSolve` retrieves the inverse from the cache.
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## Calculate the matrix inverse
        i <- solve(data, ...)
        x$setinv(i)
        i
}
