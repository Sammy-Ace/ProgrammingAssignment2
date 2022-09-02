## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matriz = matrix()) { # [>] define function with one argument 
    Matriz_Inversa <- NULL                       # [>] init  "Matriz_Inversa" as NULL
                                                 # (it will contain the inverse of the matrix "matriz")
    Asignar <- function(y) {                    
        matriz <<- y                             # [>] define the "Asignar" function, to assign new value of matrix  
        Matriz_Inversa <<- NULL                  #     in upper environment if there is a new matrix, then reset 
                                                 #     "Matriz_Inversa" to NULL
    }
    GetMatriz <- function() matriz               # [>] "GetMatriz" function -> returns value of the matrix argument
    
    AsignarInversa <- function(inverse) Matriz_Inversa <<- inverse  
    ## assigns value of Matriz_Inversa in parent environment
    GetInversa <- function() Matriz_Inversa                     
    ## gets the value of Matriz_Inversa where called
    list(Asignar = Asignar, GetMatriz = GetMatriz, AsignarInversa = AsignarInversa, GetInversa = GetInversa)
    # this last CL allows me to refer to the functions with the $ operator ;)
}


cacheSolve <- function(matriz, ...) {
    Matriz_Inversa <- matriz$GetInversa()
    if(!is.null(Matriz_Inversa)) {
        message("Obteniendo datos de Cache")
        return(Matriz_Inversa)
    }
    data <- matriz$GetMatriz()
    Matriz_Inversa <- solve(data, ...)
    matriz$AsignarInversa(Matriz_Inversa)
    Matriz_Inversa
}