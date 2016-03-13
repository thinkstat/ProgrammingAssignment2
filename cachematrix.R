## Purpose: Caching the Inverse of a Matrix (Assumption: the matrix supplied is
## always invertible)

## Step 1-Function: makeCachematrix - To create a special Matrix object that can 
## cache its inverse

## Step 2-Function: cacheSolve - To compute the inverse of the special Matrix 
## returned by makeCacheMatrix function. The cacheSolve function retrieves the 
## inverse from the cache if the inverse of the matrix has already been calculated, 
## provided the matrix has not changed.

## The first function, makeCacheMatrix returns a list containing a function to
## do the following:
##      set the matrix
##      get the matrix
##      set the inverse of the matrix
##      get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  
        ## x - Square matrix which is always invertible 
        
        m_inverse <- NULL
        set <- function(y) {
                x <<- y # <<- operator is used to assign a value to an object
                m_inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m_inverse <<- inverse
        get_inverse <- function() m_inverse
        list(set = set, get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

## The cacheSolve function returns the inverse of the input matrix from
## makeCacheMatrix function by first checking whether the inverse has already
## been calculated. If the inverse already exists, it gets the inverse from
## the cache, and if otherwise it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m_inverse <-x$get_inverse()
        
        ## To check whether the inverse has previously been calculated & skip 
        ## the inverse computation if it already exists
        
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        
        ## If inverse does not exist, then it computes the inverse
        
        matrix_data <- x$get()
        m_inverse <- solve(matrix_data, ...) ## solve:returns the inverse of a
                                             ## square matrix (Source: Quick-R)
        x$set_inverse(m_inverse)
        return(m_inverse)
}
