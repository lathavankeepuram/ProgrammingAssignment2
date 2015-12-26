## Inversion of a matrix involves costly computation. Instead of computing the inverse of a matrix repeatedly 
## it is faster and efficient to access the cached value of inverse of the matrix.   
## Below  are a pair of functions that will cache the inverse of a square matrix.
## For this assignment, it is assumed  that the matrix supplied to the function is always invertible.


## Below function makeCacheMatrix takes a square matrix (x) as input variable and returns a list that contains
## four functions - set, get, setinv, getinv

## Function set takes input variable y (a square matrix) and will return the variable invmat that is set to NULL 
## Within the function set, the value of input variable y is assigned to variable x 
## (x is input in the parent function makeCacheMatrix)

## Function get just returns the latest value of variable x

## Function setinv takes inverse of square matrix x as the input variable and returns variable invmat
## variable invmat has the inverse of matrix x (inverse) assigned to it

## Function getinv returns variable invmat (that has  inverse of matrix x assigned to it)

makeCacheMatrix <- function(x = matrix()) {
    
    invmat  <- NULL
    
    set <- function(y) {
            x <<- y
            invmat <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) invmat <<- inverse
    
    getinv <- function() invmat
    
    list( set = set, 
          get = get,
          setinv = setinv,
          getinv = getinv)

}


## Below function cacheSolve takes a list as input and returns inverse of square matrix
## The input variable x here   is a list that contains four functions - set, get, setinv,getinv, basically
## x here denotes the output of the above function makecacheMatrix
## Within this function, there is first a check done to see if the function x$getinv returns a value or a NULL
## If x$getinv returns a value, the inverse is already computed and cached so variable invmat is assigned 
## the value returned by function x$getinv and invmat is returned

## If call to x$getinv returns a NULL then inverse  of the square matrix has not been calculated yet
## So call to x$get will give the latest square matrix value and function solve will 
## calculate the inverse of this matrix. The inverse is then assigned to variable invmat
## Function x$setinv is called using invmat variable as input and this caches the inverse of matrix for future use
## Finally invmat variable (has inverse of matrix value assigned)  is returned by function cacheSolve

cacheSolve <- function(x, ...) {
      
    invmat <- x$getinv()
      
    if(!is.null(invmat)) {
        message("getting cached inverse matrix")
        return(invmat)
    }
    
    data <- x$get()
    
    invmat <- solve(data,...)
    
    x$setinv(invmat)
    
    invmat
       
}
