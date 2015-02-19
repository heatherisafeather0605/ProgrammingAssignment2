## The first function creates an empty matrix that can cache the 
## inverse of the matrix. The second function computes the inverse
## of the matrix created in makeCacheMatrix. If the martix already 
## exists, then it uses the existing cache and returns both the 
##result and the message "getting cached data"

## Creates an empty matrix in prep for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list (set=set, get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}


## Creates inverse of matrix returned

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){      ##if cache already exists, pull it and display with message
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()     ##creates inverse of matrix
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
