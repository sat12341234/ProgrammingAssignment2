## This file describes 2 functions for the course assignment
## makeCacheMatrix creates a group of functions 
##    get, getmatrix, set and setmatrix
## cacheSolve of the inverted matrix if "m" is NULL
## Ths implementation is identical to the mean example, I have used solve in place of mean.
##

####################################
## tested with following inputs
#> x <- matrix(1:4, 2, 2)
#> x
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> solve(x)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
####################################

####################################
## TEST WITH OUR FUNCTIONS
#> m <- makeCacheMatrix()
#> m$set(matrix(1:4, 2, 2))
#> cacheSolve(m)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(m)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
####################################


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
