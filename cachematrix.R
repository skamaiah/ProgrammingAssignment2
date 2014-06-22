# 1. Create a matrix by calling the function makeCacheMatrix and assigning it to variable A
#       - A <- makeCacheMatrix (c(1:4), nrow = 2, ncol = 2) 
# 2. Check the matrix that was created using the makeCacheMatrix 
#       - A$get() 
# 3. Solve for the inverse of this matrix using our second function, cacheSolve, 
#       - cacheSolve(A)
# 4. Now that you have the inverse matrix, pass the matrix A to the cacheSolve function a second time
#       - cacheSolve(A)
# inverse of matrix A is displayed with the message "Getting cached data" indicating that the inverse
#  is not being calculated this time but is being retrieved from the cached value


# makeCacheMatrix is a function that stores the inverse of a matrix to reduce recalculation of the inverse if called again 


makeCacheMatrix <- function(x = matrix(),nrow,ncol) {


  # Check to see if the function is called with a square matrix as an input, if not stop the code and populate an error message
    if(nrow!=ncol) stop("Input must be for a Square Matrix with same no. of rows & columns")
  
  # set inverse matrix store to NULL
    inv <- NULL
  
  # set x matrix to variable y
    set <- function(y){
    x <<- y
    inv <<- NULL #clear (or NULL) inv matrix in closure
    }
 
  # get - takes no arguments and returns a function that returns the initial (non-inverted) matrix
    get <- function() matrix(x,nrow,ncol)
  
  # setinverse - takes a matrix as an argument (calcuated using solve), setting this inverted matrix to the variable inv
    setinverse <- function(inverse) inv <<- inverse
  
  # getinverse - takes no arguments and returns the inverted matrix stored in the variable inv
    getinverse <- function() inv
  
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


# cacheSolve takes an object of type makeCacheMatrix as an argument and returns the inverse of that matrix


cacheSolve <- function(x, ...) {


  # collect cached inverse matrix
    inv <- x$getinverse()
   
  # test to see if matrix_inverse is not NULL (ie already has a cached inverse matrix )
    if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
    }
   
  # Generate the inverse matrix using solve function in R
    data <- x$get()
    inv <- solve(data, ...)
  
  # cache the inverse matrix
    x$setinverse(inv)
  
  # return the inverse matrix
    return(inv)
}      
