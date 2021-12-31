## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

  ## initialize two objects: x and m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set the value of the matrix.
  ## assign NULL to m object in parent environment.
  ## If cacheSolve has been executed already, m<<-NULL clears any value 
  ## of m that had been cached.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <-function() x
  
  ## set the value of the inverse matrix 
  setSolve <- function(solve) m <<- solve
  
  ## get the value of the inverse matrix
  getSolve <- function() m
  
  ## create a list naming each element created.
  ## This allows us to use the $ form of the extract operator 
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  
  ## get the value of the inverse matrix
  m <- x$getSolve()
  
  ## If the inverse has already been calculated (and the matrix 
  ## has not changed), then the cacheSolve should retrieve the 
  ## inverse from the cache and report "getting cached data".
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If the inverse hasn't been calculated (and the matrix has
  ## changed), then cacheSolve gets the matrix from the input object,
  ## calculates the inverse, sets the values of the inverse matrix
  ## and returns a matrix that is the inverse of 'x'.
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}