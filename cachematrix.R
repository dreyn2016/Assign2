## Below are two functions that are used to create a special object that stores a 
## square invertible matrix, caches its inverse, and either computes or retrieves
## its inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invertedMatrix <- NULL
  set <- function(y){
    x <<- y
    invertedMatrix <<- NULL
    
  }
  get <- function() x
  setinvertedMatrix <- function(solve) invertedMatrix <<- solve
  getinvertedMatrix <- function() invertedMatrix
  list(set = set, get = get, 
       setinvertedMatrix = setinvertedMatrix, 
       getinvertedMatrix = getinvertedMatrix)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  invertedMatrix <- x$getinvertedMatrix()
  if(!is.null(invertedMatrix)) {
    message("getting inverted matrix cached data")
    return(invertedMatrix)
  }
  data <- x$get()
  invertedMatrix <- solve(data, ...)
  x$setinvertedMatrix(invertedMatrix)
  invertedMatrix
  ## Return a matrix that is the inverse of 'x'
}
