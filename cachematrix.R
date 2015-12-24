## This project preforms and caches the inverse of a matrix
## to avoid repeated (costly) computations

## THis function creates a special matrix object that 
##can hold a cache of its inverse

##Version 1.0, Dec 24 2015. Author: ElarX. Course: Coursera R Programming

makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL                         #set original inverse value to non-existent if it's not computed
    set <- function (input){               #sets a matrix to this object and inverse to null
        x<<- input
        inverse = NULL
    }
    get <- function() x                            #returns the matrix
    setInverse <- function(input) inverse <<-input  #set inverse of matrix to cache
    getInverse <- function() inverse                #return cached inverse of matrix
    #return a list containing contents of the special matrix object
    list(set = set, get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


## This is a funciton that checks if there is already an inverse that
## exists for a given matrix. If not, it computes it by calling
## on an the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()             #get inverse if available
        if(!is.null(inverse)){                #if an inverse is present, return it
            message("getting cached data")
           return(inverse)
        }else{                                #if no cache present, compute it and set it
        tempMatrix <- x$get()                 
        inverse <-solve(tempMatrix)           #solve the matrix
        x$setInverse(inverse)                 #cache the solutin into the x object
        inverse                               #return the inverse of the matrix
        
        }
}

