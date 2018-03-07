## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix uses a matrix m and returns a list with functions.
## each function can be called using $set(), $get(), $setInverse() and 
## $getInverse() on the variable name you stored makeCacheMatrix(m) in.
## comments explain what each variable used in the function does.
## args:
##              x: matrix (OPTIONAL)
## returns:
##              list of functions to get/set value & get/set inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # sets m to NULL in this function environment
        
        set <- function(y) { # Not used but can set new x and sets m to NULL in
                x <<- y      # the function environment. m2$set(m) for instance.
                m <<- NULL   # If removed still works, also remove it in the 
        }                    # output list
        
        get <- function() x # calls x from this function environment, is the matrix
        # used in function argument
        
        setInverse <- function(inverse) m <<- inverse # when used stores variable 
        # declared "inverse" into 
        # "m" in the parent 
        # environment of this
        # function
        
        getInverse <- function() m # calls m from this function environment 
        
        list(set = set, get = get,    # puts the variables into a list that can  
             setInverse = setInverse, # be called with by the $
             getInverse = getInverse)
}

## Write a short comment describing this function
## This function returns the inverse of a specified matrix and stores
## the output into the functions memory. If used again it can quickly
## output it from memory instead of calculating it again.
## args:
##              x: matrix (OPTIONAL)
##              ...: Extra arguments
## returns:
##              1st use the inverse of the matrix from calculation
##              after 1st use the inverse of the matrix from memory + message
##              "already did this"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() # gets the variable m stored in the"makeCacheMatrix" 
        # environment stored in m2
        
        if(!is.null(m)) {                   # if this is TRUE the m value in
                message("already did this") # the "makeCacheMatrix" environment
                return(m)                   # stored in m2 has been overwritten 
        }                                   # with the value m from this function's 
        # environment. (x$setInverse(m)) and 
        # is not longer "NULL"
        
        data <- x$get() # puts the value from "x$get()" which is the matrix x 
        # from the "makeCacheMatrix" environment stored in m2
        
        m <- solve(data, ...) # uses the data variable and inverses the data frame
        
        x$setInverse(m) # overwrites the m in the "makeCacheMatrix" environment stored in m2
        
        m # prints m to the console
}

## Example
# m <- matrix(c(2,1,1,2), nrow = 2, ncol = 2, byrow = TRUE)
#      [,1] [,2]
# [1,]    2    1
# [2,]    1    2
# 
# m2 <- makeCacheMatrix(m)
# cacheSolve(m2)
#            [,1]       [,2]
# [1,]  0.6666667 -0.3333333
# [2,] -0.3333333  0.6666667
# cacheSolve(m2)
# already did this
#            [,1]       [,2]
# [1,]  0.6666667 -0.3333333
# [2,] -0.3333333  0.6666667