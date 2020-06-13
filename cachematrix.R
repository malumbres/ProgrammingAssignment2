## Coursera Assignment2
## Functions to create a special “matrix” object that can cache its inverse, and computation
## of the inverse of the special “matrix” generated

## makeCaheMatrix. Generates a matrix object  and the function to get its inverse
makeCacheMatrix <- function(x = matrix()) {
  mm <- NULL
  set <- function(y){
    x <<- y
    mm <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) mm <<- inverse
  getInverse <- function() mm 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve. Solves the inverse using cache from the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mm <- x$getInverse()
  if(!is.null(mm)){
    return(mm)
  }
  mat <- x$get()
  mm <- solve(mat,...)
  x$setInverse(mm)
  mm
}
