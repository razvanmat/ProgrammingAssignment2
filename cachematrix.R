## Shows the inverse of a square & invertable matrix
## functions do



## Make sure that the argument of makeChacheMatrix is applied in the following form:
## "randomObject" <-makeCacheMatrix(matrix(rnorm(9,ncol=3,nrow=3), ncol=3,nrow=3))

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
## names the elements in the list so that you can call them using the "$" operator
}


## Function tests if the matrix is squared & if it has an inverse => if both conditions are not simultaneously TRUE 
## then you will be prompted to enter a new matrix value in to the makeCacheMatrix function.
## For values which satisfy the two rules mentioned above, the cachSolve function returns the inverse of the matrix

cacheSolve <- function(x, ...) {

      ##test to see if the matrix is squared
      if (ncol(x$get())/nrow(x$get())!=1) {
            print("Your matrix is not squared, please use the 'yourobject'$set('new matrix') function and create a new matrix with the arguments nrow being equal to ncol")
            
      }
      else {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      ## checks the inverse variable => if its not null then it uses the inverse matrix stored in the memory
            }
      data <- x$get()
      inverse <- tryCatch(solve(data, ...),error = function(e) {inverse = ("Your matrix can't be inverted, please try different matrix values using the '$set' function")})
      ## handleing the cases where the matrix has no inverse sollution (eg. matrix(1:9,3,3); matrix(rep(3,9),3,3))
      x$setinverse(inverse)
      inverse
      }
}
## Example:
## TestMatrix<-makeCacheMatrix(matrix(c(1,-2,3,1),ncol=2,nrow=2))
## cacheSolve(TestMatrix) => calculates and returns the inverse of the matrix
## cacheSolve(TestMatrix) => calls the 'inverse' variable which contains the cached inverse of the matrix


##Enjoy!
