##Catching the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) { ## define the argument
  inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {                    ## define the set function to assign new value of the matrix
    x <<- y                             
    inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                   ## define the get fucntion to return the value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
##Compute the inverse of the matrix
cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  ##Assign the inverse value of x to inv
  if(!is.null(inv)) {     ##If inv is not null, return the message and the value of inv
    message("getting cached data")
    return(inv)
  }
  data <- x$get()   ##Assign value to data which is the value of the matrix argument
  inv <- solve(data, ...) ##assign value to inv 
  x$setinverse(inv) ##return the inverse of the matrix
  inv
}
