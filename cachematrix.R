## This script file is being written in conjunction with the R Programming course (rprog-005)
## It is programming assignment 2 and will be tied to the programming Assignment 2 repository
## forked appropriately from GitHub

## the makeCacheMatrix function below will create a specialized matrix list/object used in
## conjunction with the cacheSolve function below.

## This function can be called with no arguments and assigned to a variable to create a blank
## list/object which can be set using the two set functions
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        #sets the matrix in this object
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #returns the matrix set by setMatrix()
        getMatrix <- function() {
                return(x)
        }
        
        #sets the inverse of the matrix to be calculated by the cacheSolve function 
        setInv <- function(inverse) {
                inv <<- inverse
        }
        
        #returns the inverse of the matrix
        getInv <- function() {
                inv
        }
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInv = setInv,
             getInv = getInv)
}


## the cacheSolve function takes a list/object created by the makeCacheMatrix function above
## and calculates the inverse of the function and returns said inverse if it isn't calculated
## already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        inv <- x$getInv()
        if (!is.null(inv)) { #check to see if makeCacheMatrix already contains inverse
                message("Retrieving cached inverse.")
                return(inv)
        }
        inv <- solve(x$getMatrix(), ...) #calculates inverse if not already cached
        x$setInv(inv) #sets inverse in makeCacheMatrix list/object to calculated value above
        inv #returns the inverse
}
