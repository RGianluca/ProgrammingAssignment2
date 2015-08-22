## Function makeCacheMatrix()
# makeCacheMatrix() is a function that returns a list of 4 functions:
# set(): sets the x ("direct") square matrix according to the given input
# get(): returns the x matrix
# setinv(): sets the inverse matrix inv_x (i.e. x^-1) to the given input
# getinv(): returns the inverse matrix inv_x (i.e. x^-1)

makeCacheMatrix <- function(x = matrix()) {
        # Initialise the inverse matrix to NULL
        inv_x <- NULL
        # Definition of the set() function
        set <- function(y = matrix()){
                x <<- y
                inv_x <<- NULL
        }
        # Definition of the get() function 
        get <- function() x
        # Definition of the setinv() function 
        setinv <- function(inversa = matrix()) inv_x <<- inversa
        # Definition of the getinv() function
        getinv <- function() inv_x
        # the following lines builds the list of functions, i.e. the output of the function makeCacheMatrix()
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}

#---------------------------------------------------------------------------------------------
## Function cacheSolve()
# The cacheSolve() function computes the inverse of the x matrix assuming that it is invertible
# It requires as input the "list of functions" returned by the makeCacheMatrix() function
# Example of usage
# initial_matrix <- cbind( c(1,2), c(3,1))
# list_of_functions <- makeCacheMatrix(initial_matrix)
# cacheSolve(list_of_functions)
#

cacheSolve <- function(x, ...) {
        # First it gets the value of the inverse matrix using the getinv() funtion
        inv_x <- x$getinv()
        # if the variable that stores the inverse matrix is not NULL, that means that
        # the inverse matrix has already been computed and the function returns it
        if(!is.null(inv_x)){
                message("getting cached inverted matrix")
                return(inv_x)
        }
        # otherwise it computes the inverse and returns it
        matrice <- x$get()
        inv_x <- solve(matrice)
        inv_x
}
