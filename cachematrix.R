## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix, it set and get a matrix 
## as well as set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { # the function argument is a matrix
  i <- NULL                                 # initialize i which later will contain the inverse matrix
  set <- function(y = matrix()) {           # sets the matrix
    x <<- y                                 # assign the value of y to x in an environment 
    i <<- NULL                              # reset i to NULL
  }
  get <- function() x                       # gets the value of x which in this case is the matrix
  setinv <- function(inv = matrix()) {      # sets the inverse matrix
    i <<- inv                               # assign the inverse matrix value to i
  }
  getinv <- function() i                    # gets the inverse matrix
  list(set = set, get = get,                # stores all 4 functions in a list
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function above. 
## If the inverse has already been calculated,then the cachesolve function retrieves the inverse from the cache.
## Otherwise, it calculates the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()                           # assign i with the inverse matrix values return from the getinv function
  if(!is.null(i)) {                         # If the value retrieved from the getinv function is not null then
    message("getting cached data")          # the inverse matrix exists in the cache and it will be returned
    return(i)
  }
  data <- x$get()                           # else if the value retrieved is NULL, then it will get the matrix
  i <- solve(data, ...)                     # and calculate the inverse by using the solve() function
  x$setinv(i)                               # sets the inverve matrix
  i                                         # return the invers matrix
}
