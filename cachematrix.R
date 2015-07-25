## These functions allow to cache inversion of given matrix for fastening repeatable computations

## makeCacheMatrix function makes from given matrix object (list) who can store original matrix and cache it's inversion
## arguments: x of type "matrix"

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  set_inv_matrix <- function(inv_matrix) inv_m <<- inv_matrix
  get_inv_matrix <- function() inv_m
  list(set = set, get = get,
       set_inv_matrix = set_inv_matrix,
       get_inv_matrix = get_inv_matrix)
}


## cacheSolve function returns inversion of given cachable matrix object (created with makeCacheMatrix)
## If inversion already computed it'll be returned from stored value
## arguments: x of type, returned by makeCacheMatrix, other argument will be passed to solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_m <- x$get_inv_matrix()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$set_inv_matrix(inv_m)
  inv_m
}
