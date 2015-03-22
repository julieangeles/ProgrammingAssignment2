makeCacheMatrix <- function(x = matrix()) {
  i <- NULL               
  ##set inverse of a matrix to null

  ## set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get matrix
  get <- function() x           
  ## setting inverse matrix
  setinvmatrix <- function(imatrix) 
    i <<- imatrix
  ## getting inverse matrix
  getinvmatrix <- function() i                 
  
  ## list names
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## cacheSolve computes the inverse of the matrix 
cacheSolve <- function(x, ...) {
  
  ## imatrix value
  i <- x$getinvmatrix() ## gets the invmat in previous function
  
  ## if inverse matrix is already stored, get cached data and return the inv matrix
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if not, calculate inverse of the matrix 
  data <- x$get()              ## get the matrix
  i <- solve(data, ...)      ## calculate the inverse
  x$setinvmatrix(i)             ## set the inverse of the matrix
  i                         ## print the inverse of the matrix

}

