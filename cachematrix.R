
#  this function takes a matrix as an input and calculates it's inverse.

makeCacheMatrix <-  function(x = matrix()) {
      # "matr" will be my matrix's inverse, by default it's blank
      matr <- NULL
      
      set <- function(y) {
            x <<- y
            matr <<- NULL
      }
      # "get" will give back the original matrix
      get <- function() x
      # "setsolve" will create the inverse of my matrix in matr 
      # and make it available in other environments
      setsolve <- function(solve) matr <<- solve
      # "getsolve" will give back the result of matr
      getsolve <- function() matr
      # the output of this function will be the list of the functions described above
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

# this function computes the inverse of a matrix, if the inverse had been calculated before 
# it gives back the result from the cache. The function uses the result of makeCacheMatrix 
# function.
cacheSolve <- function(x, ...) {
      # first I get the result from makeCacheMatrix function
      imatrix <- x$getsolve()
      # if it's not null then I call the data from cache and you'll see a message with the result
      if(!is.null(imatrix)) {
            message("getting cached data")
            return(imatrix)
      }
      # if data is new or the matrix has been changed I calculate the inverse 
      # of the matrix and print it
      data <- x$get()
      imatrix <- solve(data, ...)
      x$setsolve(imatrix)
      imatrix
}