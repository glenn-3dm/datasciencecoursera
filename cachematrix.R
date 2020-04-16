## makeCacheMatrix will create a matrix and cache its inverse

## With x as a blank matrix, the user will pass in the matrix variable to inverse and cache

makeCacheMatrix <- function(x = matrix()) {
      MI <- NULL
      set <- function(y)  {
            x <<- y
            MI <<- NULL
      }
      get <- function() x
      setMI <- function(solve)  MI <<- solve
      getMI <- function() MI
      list(set = set,get = get,setMI = setMI,getMI=getMI)
}


## Check the cache to see if the inverse of the matrix has already been saved
## If saved, return saved inverse.  If not, calculate and return inverse

cacheSolve <- function(x, ...) {
        MI <- x$getMI()
        if(!is.null(MI))  {
              message('retrieving cache data...')
              return(MI)
        }
        data <- x$get()
        MI <- solve(data,...)
        x$setMI(MI)
        MI
}
