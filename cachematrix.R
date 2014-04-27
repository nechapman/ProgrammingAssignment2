## Put comments here that give an overall description of what your
## functions do
## The two functions in the script below represent a pair that compute and cache an inverse of a matrix. 
## Together, they ensure an inverse of a matrix needs only be computed once.


## Write a short comment describing this function
## Function with input matrix x, and three subfunctions: set_matrix (sets input
## matrix x as matrix), get_matrix (returns input matrix x), set_inverse (caches 
## an inverse of a matrix) and get_inverse (returns inverse of a matrix if it 
## exists, otherwise it returns NULL. 
makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set_matrix <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inverse_matrix) inverse_matrix <<- inverse_matrix
    get_inverse <- function() inverse_matrix
    s <- list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}



## Write a short comment describing this function
## Function with input matrix x, that returns inverse of input matrix: either by 
## getting it from cache, or computing it (and in that eventuality, storing it.)
cacheSolve <- function(x, ...) {
    inverse_matrix <- x$get_inverse()
    if(!is.null(inverse_matrix)) {
        message("Inverse matrix exists, so getting cached inverse.")
        return(inverse_matrix)
    }
    m <- x$get_matrix()
    inverse_matrix <- solve(m)
    x$set_inverse(inverse_matrix)
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix
}

