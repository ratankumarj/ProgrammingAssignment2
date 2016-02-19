## Put comments here that give an overall description of what your
## Write a short comment describing this function
# makeCacheMatrix is a function to define a special 
## type of function to store inverse of matrix
## First call makeCacheMatrix() to create the special type of matrix
## before calling set function to assign the values
## Write a short comment describing this function

  makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) 
       {
        ## Check y is a matrix.
        if ( class(y) == 'matrix') 
            {
              x <<- y
              m <<- NULL 
             }
        ## Print Error Message if a matrix is not supplied as input
        else print("Input variable not a matrix. Please provide matrix as input")
       }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
## This function returns inverse of matrix. This accomplishes by
## first checking whether inverse has been already calculated
## It has to check that matrix has not changed since the last calculation
## To call this function first the matrix has be created by calling 
## makeCacheMatrix function
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      ## Check if the inverse has already been calculated
      if(!is.null(m)) 
        {
          message("getting cached data")
          return(m)
        }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}
# End
