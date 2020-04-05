calculator <- function() {
  
  add <- function(x, y) {
    return(x + y)
  }
  
  subtract <- function(x, y) {
    return(x - y)
  }
  
  multiply <- function(x, y) {
    return(x * y)
  }
  
  divide <- function(x, y) {
    return(x / y)
  }
  
  factors <- function(x)
  {
    print(paste("The factors of",x,"are:"))
    for(i in 1:x) {
      if((x %% i) == 0) {
        print(i)
      }
    }
  }
  
  prime <- function(x)
  {
    if (x == 2) {
      print(paste(x,"is a prime number."))
    } else if (any(x %% 2:(x-1) == 0)) {
      print(paste(x,"is not a prime number."))
    } else { 
      print(paste(x,"is a prime number."))
    }
  }
  

  
  print("******Simple R Calculator - Select operation: ******")
  print("1.Add")
  print("2.Subtract")
  print("3.Multiply")
  print("4.Divide")
  print("5.Factors")
  print("6.Prime")
  choice = as.integer(readline(prompt="Enter choice[1/2/3/4/5/6]: "))
  
  if (choice == 5 | choice == 6) {
    num1 = as.integer(readline(prompt="Enter number: "))
  } else {
    num1 = as.integer(readline(prompt="Enter first number: "))
    num2 = as.integer(readline(prompt="Enter second number: "))
  }
  
  operator <- switch(choice,"+","-","*","/")
  result <- switch(choice, add(num1, num2), subtract(num1, num2), multiply(num1, num2), divide(num1, num2), factors(num1), prime(num1))
  
  if (choice <= 4) {
    print(paste(num1, operator, num2, "=", result))
  }
  
}

source("calculator.R")

calculator()


