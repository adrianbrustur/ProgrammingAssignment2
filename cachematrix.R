## Name:        makeCacheMatrix 
## Purpose:     This is a function to be used in conjunction
##              with the 'cacheSolve' function.
##              The function creates a list of functions to:
##              - set the matrix
##              - get the matrix
##              - set the inverse of the matrix
##              - get the inverse of the matrix
## Input:       a non-singular square matrix
## Returns:     the above list of functions
## Author:      Adrian-George Brustur
## Version:     1.0
## Date:        May 2015

makeCacheMatrix <- function(x = matrix()) {
        # The 'inv' variable stores the cached
        # value of matrix 'x'
        inv <- NULL
        
        # Set the matrix 'x'
        set_matrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get matrix 'x'
        get_matrix <- function() x
        
        # Set the value of the inverse of matrix 'x'
        # This assumes that the inverse has been calculated
        # and is now cached for further reference in the future
        set_matrix_inverse <- function(inverse) inv <<- inverse
        
        # Get the inverse of matrix 'x' that has been cached earlier on
        get_matrix_inverse <- function() inv
        
        list(set.matrix = set_matrix, get.matrix = get_matrix,
             set.matrix.inverse = set_matrix_inverse,
             get.matrix.inverse = get_matrix_inverse)
}

## Name:        cacheSolve 
## Purpose:     Improves the computation of the inverse of a 
##              non-singular square matrix 'x' by caching its inverse.
##              The function is to be used in conjunction with the 
##              function 'makeCacheMatrix'.
##
## Input:       list created with the 'makeCacheMatrix' function
## Returns:     inverse of the matrix used as input for 'makeCacheMatrix'
## Author:      Adrian-George Brustur
## Version:     1.0
## Date:        May 2015
## Example of usage:
##
## > x<-matrix( c(2, 4, 3, 1, 5, 7, 9, 2, 5), nrow=3, ncol=3)
## > inverse.of.x<-cacheSolve(makeCacheMatrix(x))

cacheSolve <- function(x, ...) {
        ## Attempt to get the inverse of matrix 'x'
        inv <- x$get.matrix.inverse()
        
        ## If the inverse of 'x' exists, return it
        if(!is.null(inv)) {
                message("Returning the inverse...")
                return(inv)
        } else
                
        ## If the inverse of 'x' does not exist
        ## calculate it, cache its value
        ## and return it
        {
                temp.matrix <- x$get.matrix()
                inverse <- solve(temp.matrix)
                x$set.matrix.inverse(inverse)
                inverse  
        }

}
