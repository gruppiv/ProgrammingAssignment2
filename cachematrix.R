## The goal is to make a more efficient code to calculate the inverse
## of a given matrix by making a cache data, if the inverse was already
## calculated. Some of the variables was named in Portuguese: 
## 'Estab' is a acronym of 'Estabelecer', equivalent to 'set'
## 'Obter' is a verb that means 'get'
## The other names are maintain in english

## The following function (makeCacheMatrix) receive a matrix and return a 
## list that contains functions. Which one storage a parameter that can be  
## modified by the '<<-' operator. This operator make an assignment to the 
## variable in the parent environment, even if is in a child environment.

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        print(Inverse)
        Estab <- function(y){
                x <<- y
                Inverse <<- NULL
        }
        Obter_Matrix <- function() x
        Estab_Inverse <- function(solve) Inverse <<- solve
        Obter_Inverse <- function() Inverse
        list(Estab = Estab, Obter_Matrix = Obter_Matrix,
             Estab_Inverse = Estab_Inverse,
             Obter_Inverse = Obter_Inverse)
}


## cacheSolve function will modify the makeCacheMatrix list. First, it checks
## if the inverse was already calculated. If so, they return the cache data 
## in 'Inverse' variable without compiling again the same code. If not, the 
## function calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        Inverse <- x$Obter_Inverse()
        Exists <- !is.null(Inverse)
        if(Exists){
                message("getting the cache data...")
                return(Inverse)
        }
        Matrix <- x$Obter_Matrix()
        Inverse <- solve(Matrix, ...)
        x$Estab_Inverse(Inverse)
        Inverse
        ## Return a matrix that is the inverse of 'x'
}
