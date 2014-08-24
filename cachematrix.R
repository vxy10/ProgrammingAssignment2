## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix, takes a matrix and caches its value, and inverse
## cacheSolve, takes cached matrix as input, and returns inverse if its cached, if not, computes it.

makeCacheMatrix <- function(x ) {
  # Checking if input is a matrix
  if (!is.matrix(x)) {
    message("Input must be a matrix.")
    return
  }
  # Checking input is a square matrix
  dim_x = dim(x) #dimension of input 
  if (dim_x[1]!=dim_x[2]) {
    message("Inverse can be computed for square matrices only.")
    return
  }
  
  
  # setting initial value
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  # get function, returns x
  get <- function() x
  setinv <- function(inv_set) inv_x <<- inv_set # sets inverse
  getinv <- function() inv_x  # gets inverse
  
  # making list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns inverse of the matrix.

cacheSolve <- function(x, ...) {
  # setting inverse (inv_x) to value stored in x.
  inv_x <- x$getinv() 
  mat_x <- x$get()
  
  dim_x = dim(mat_x) # dimension of matrix
  
  if (dim_x[1]!=dim_x[2]) {
    message("Inverse can be computed for square matrices only.")
    
    return
  }
  
  # if inverse is calculated, return cached value.
  if(!is.null(inv_x)) {
    
    # this part checks if stored inverse is the actual inverse
    eye_x = diag(dim_x[1]) # making identity matirx
    err_x = eye_x - inv_x%*%mat_x # definining error as I - inv(X)*X
    
    # if error is less than threshold (10^-8),return stored value.
    if (norm(err_x)<10^-8){
      message("getting cached data")
      return(inv_x)
    }
    message("Recomputing inverse")
  }
  # this part executed only if inverse is not calculated, 
  # or the stored inverse is not actual inverse.
  data <- x$get() # getting data
  inv_x <-solve(data) #solving for inverse
  x$setinv(inv_x) #setting inverse
  
  inv_x
}