## Please find comments explaining each part of the code below


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  
  matrix <- NULL  # Assign matrix object as null initially
  
  set <- function(y) {	## Set the matrix Globally as "x"
    x <<- y
    m <<- NULL
  }
  get <- function() x	## Return value of input symbol
  setInv <- function(solve) matrix <<- solve	## Creates setInv function for caching the input to the symbol "matrix"
  getInv <- function() matrix	## Function to return value of "matrix"
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)	## List of available functions indicated
}
cacheSolve <- function(x, ...) {
  ## This function cache's the matrix inverse to be quiered at any later stage without having to recalculate
  matrix <- x$getInv()	## Gets the value of matrix. This is equal to "NULL" if not previously run
  if(!is.null(matrix)) {	## Ran if "matrix" doesn't equal NULL
    message("getting cached data")	##Message on output
    return(matrix)	## Returns the cache value of matrix
  }
  data <- x$get()	## Assigns the input as "data"
  matrix <- solve(data, ...)	## Finds the inverse of the matrix, and assigns the value you "matrix"
  x$setInv(matrix)	## Saves "matrix" to the cache
  matrix	## Returns the value of "matrix"
}