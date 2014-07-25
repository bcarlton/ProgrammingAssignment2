## This script file is being written in conjunction with the R Programming course (rprog-005)
## It is programming assignment 2 and will be tied to the programming Assignment 2 repository
## forked appropriately from GitHub

## the makeCacheMatrix function below will create a specialized matrix list used in
## conjunction with the cacheSolve function below.  Said list employs functions much in the same
## way methods are employed in class definitions in other, more object-oriented programming
## centric languages

## makeCacheMatrix can be called with no arguments and assigned to a variable to create a blank
## CacheMatrix list which can be future modified using the setMatrix and setInv functions
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # sets the matrix in this object
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # returns the matrix set by setMatrix function
        getMatrix <- function() {
                return(x)
        }
        
        # sets the inverse of the matrix to be calculated by the cacheSolve function 
        setInv <- function(inverse) {
                inv <<- inverse
        }
        
        # returns the inverse of the matrix set by setInv function
        getInv <- function() {
                inv
        }
        
        # return list of all functions defined above; this is what the cacheSolve function
        # will operate upon
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInv = setInv,
             getInv = getInv)
}


## the cacheSolve function takes a list created by the makeCacheMatrix function above
## and calculates the inverse of the function and returns said inverse if it isn't calculated
## already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        inv <- x$getInv()
        if (!is.null(inv)) { #check to see if CacheMatrix list already contains inverse
                message("Retrieving cached inverse.")
                return(inv)
        }
        inv <- solve(x$getMatrix(), ...) #calculates inverse if not already cached
        x$setInv(inv) #sets inverse in a CacheMatrix list to calculated value above
        inv #returns the inverse
}
