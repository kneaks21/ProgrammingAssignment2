##Programming in R: Week 2 Assignment
##Nicole Bobbitt 2016-05-01

############################   makeCacheMatrix  ###############################
## Creates a special object that contains each of the functions defined and each
## of the variables stored with the <<- assignment. This way each of these 
## variables and functions will be stored in memory. 

makeCacheMatrix <- function(storedmatrix = matrix()) {
        inverse <- NULL                          
        setM <- function(y){                          
                storedmatrix <<- y                            
                inverse <<- NULL                 
        }
        getM <- function() storedmatrix          
        setinverseM <- function(solve) inverse <<- solve
        getinverseM <- function () inverse 
        list(setM = setM, getM = getM, 
             setinverseM = setinverseM, 
             getinverseM = getinverseM)
}

##############################   cacheSolve  ##################################
## Function looks at the input and checks to see if the mean has already been
## calculated and stored by makeCacheMatrix function. If not, it uses the 
## functions created in makeCacheMatrix to compute the mean of the input. 
## Either way, it will print the mean of the input. 

cacheSolve <- function(x, ...){
        inverse <- x$getinverseM()
        if(!is.null(inverse)){
                message("getting cahced data")
                return(inverse)
        }
        data <- x$getM()
        inverse <- solve(data,...)
        x$setinverseM(inverse)
        inverse
}