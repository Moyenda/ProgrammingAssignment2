## Put comments here that give an overall description of what your
## functions do

## As you'd expect, these functions are very similar to the example funtions.
## In combination they meet the specification's requirements of:

## 1 - Creating a 'matrix object' (actually a list of functions) that can be  
## used to cache the inverse of the matrix argumet 'x' in the variable 'i'.

## 2 - Creating a function that returns the inverse of a matrix if the inverse
## has not already been calculated and cached (in which case it returns the 
## cached value instead).

## Write a short comment describing this function

## The first function 'sets the scene' by creating a list of functions that can 
## be used to set/get the value of the matrix value 'x' and the variable for 
## the inverse of that value 'i'.

## The variable 'i' is initiallised with a NULL value.

## The '<<-' opperator is used to enable the functions 'setM' and 'setI' to 
## affect the value of variables outside of the environments of these functions.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setM <- function(y){
                x <<- y
                i <<- NULL
        }
        getM <- function() x
        setI <- function(inv) i <<- inv
        getI <- function() i
        list(setM = setM, getM = getM, setI = setI, getI = getI)
}


## Write a short comment describing this function

## This function returns a matrix that is the inverse of 'x'. To do this it:

## 1 - Checks for a non-NULL cached value of the inverse of 'x' and returns 
## this if it finds one.

## 2 - If the cache variable == NULL, the function gets the value of 'x', 
## inverts this value and sets the cache variable 'i' equal to the inversion.
## The function then returns the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getI()
        if (!is.null(inverse)) {
                print('Retrieving cached data.')
                print(inverse)
                return
        }
        matrix <- x$getM()
        flip <- solve(matrix, ...)
        x$setI(flip)
        flip
}
