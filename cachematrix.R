#Assignment2

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL  #restores inverse matrix value to null, as new inverse is recalculated.
  }    ##Changes the matrix stored in main function -- only used if matrix is chaned.
  
  get <- function() x   ##returns matrix x strored in main function
  setmatrix <- function(solve) m <<- solve   ##calculates inverse of matrix(x) and stores it
  getmatrix <- function () m   ##gets inverse of matrix(x)
  list(set = set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


cacheSolve <- function (x=matrix(), ...) {    #stores makeCacheMatrix
  m <- x$getmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)      ##returns inverse of matrix if not null and in cache
  }
  matrix <- x$get()
  m <- solve(matrix, ...)  ## solves inverse of new matrix
  x$setmatrix(m)           ## stores invversof of new matrix in cache     
  m
}