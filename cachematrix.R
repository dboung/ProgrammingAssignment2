##This code consist of two functions that return the inverse matrix of a matrix


##The first function makeCacheMatrix, take a matrix input (eg: matrix(1:4,2,2)).
## It should be assigned to a variable, so that it can be called later (eg: a)
## There are 4 things user can do with a
## a$get returns the matrix
## a$set requires an input of a new matrix that user want to set to a
## a$getInverse return the Inverse matrix
## a$setInverse requires an input of a new matrix to replace the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  #set value of matrix
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  #get value of the matrix
  get <- function() x
  #set the value of Inverse matrix
  setInverse <- function(InverseMatrix) Inverse <<- InverseMatrix
  # get value of Inverse Matrix
  getInverse<- function() Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function cacheSolve will solve for the inverse matrix of matrix
## it takes matrix as an arguement (or you can input a (see above comment))
## This function will return the inverse matrix

cacheSolve <- function(x, ...) {
  Inverse <- x$getInverse()
  
  #If value of Inverse Matrix is set, then return the value
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  
  #If it is not
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setInverse(Inverse)
  Inverse
} 
