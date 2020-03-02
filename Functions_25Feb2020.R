# Function structure and use
# 25 Feb 2020

# -------------------------------------------------------------------------


# everything in R is a function
sum(3,2) # "prefix function"
3 + 2 # an "operator", but is actually a function
`+`(3,2) # rewritten as an "infix" function

y <- 3
print(y)

`<-`(yy,3)
print(yy)

# to see contents of a function, print it
print(read.table) #print a function shows the underlying code

sd          # how is the sd calculated
sd(c(3,2))  # call function with paramters
sd()


# The anatomy of a user-defined function ----------------------------------

function_name <- function(par_x=default_x,
                          par_y=default_y,
                          par_z=default_z) {
  #function body
  #lines of r code and annotation
  #may call other functions
  # can create functions 
  # create local variables
  
  
  return(z) #returns from the function a single element (final line of code)
}
  #par_x/y/z are the names of parameters while default_z/y/z are the variables which will later be defined. { denotes start of the code}.
function_name #prints contents of function
function_name()
function_name(par_x=my_matrix,
              par_y="order",
              par_z=1:10)
# use prominent hash fencing around your function code

# give a header with function name, description of input,output

# names inside functions can be short

# functions should be short and simple, usually no more than 1 screen of text

# if too complex, break into multiple functions

#provide default values for all input parameters; it will work without giving inputs

#make default values from random number generators

#############################################
# FUNCTION: hardy_weinberg
# input: an allele frequency p (0,1)
# output: p and the frequencies of genotypes AA AB BB

# -------------------------------------------------------------------------

hardy_weinberg <- function(p=runif(1)){
  q <- 1 - p
  f_AA <- p^2
  f_AB <- 2*p*q
  f_BB <- q^2
  vec_out <- list(p=p,
                  f_AA=f_AA,
                  f_AB=f_AB,
                  f_BB=f_BB)
  return(vec_out) #output
}
##################################################
hardy_weinberg() #print of output with random runif() p value
hardy_weinberg(p=0.5) #print output with specific parameter in mind
pp <- 0.6
hardy_weinberg(p=pp) # use variable as parameter
print(pp)
print(hardy_weinberg(p=pp))

#############################################
# FUNCTION: hardy_weinberg2
# input: an allele frequency p (0,1)
# output: p and the frequencies of genotypes AA AB BB

# -------------------------------------------------------------------------

hardy_weinberg2 <- function(p=runif(1)){
  if(p > 1.0 | p < 0.0){
    return("Function failure: p must be <=1 and >=0")
    }#if p is greater than 1 or less than zero, then execute this code which provides "" error message; return() exits function and will not execute the rest of the code
  q <- 1 - p
  f_AA <- p^2
  f_AB <- 2*p*q
  f_BB <- q^2
  vec_out <- list(p=p,
                  f_AA=f_AA,
                  f_AB=f_AB,
                  f_BB=f_BB)
  return(vec_out) #output
}
###########################################
hardy_weinberg2()
hardy_weinberg(1.1)
hardy_weinberg2(1.1)
z <- hardy_weinberg2(1.1)
z 
# use "stop" function for true error trapping
#############################################
# FUNCTION: hardy_weinberg
# input: an allele frequency p (0,1)
# output: p and the frequencies of genotypes AA AB BB

# -------------------------------------------------------------------------
hardy_weinberg3 <- function(p=runif(1)){
  if(p > 1.0 | p < 0.0){
    stop("Function failure: p must be <=1 and >=0")
  }#if p is greater than 1 or less than zero, then execute this code which provides "" error message; stop() will actually return an error message
  q <- 1 - p
  f_AA <- p^2
  f_AB <- 2*p*q
  f_BB <- q^2
  vec_out <- list(p=p,
                  f_AA=f_AA,
                  f_AB=f_AB,
                  f_BB=f_BB)
  return(vec_out) #output
}
#####################################333
hardy_weinberg3(1.1)
z <- hardy_weinberg3(1.1)


# -------------------------------------------------------------------------

# global variables: visible in all parts of the code; declared in main body of program

# local variables: visible only within the function: either declared in the function or past to it through input parameters

# functions can "see" variables in global environment
# global environment cannot "see" variables in the function environment 

my_func <- function(a=3,b=4) {
  z <- a + b
  return(z)
}
my_func()

my_func_bad <- function(a=3) {
  z <- a + b
  return(z)
}
my_func_bad()
b <- 100
my_func_bad()


my_func_ok <- function(a=3){
bb <- 100  
z <- a + bb
return(z)
}
my_func_ok()
print(bb)

##################################################
# FUNCTION: fit_linear
# fits simple regression line
# inputs: numeric vectors of the predictor (x) and response (y)
# output: slope and p-value from regression

# -------------------------------------------------------------------------

fit_linear <- function(x=runif(20),
                       y=runif(20)){
    my_mod <- lm(y~x)
    my_out <- list(slope=summary(my_mod)$coefficients[2,1],
                         p_val=summary(my_mod)$coefficients[2,4])
    plot(x=x,y=y)
    return(my_out)
}
fit_linear()

# -------------------------------------------------------------------------


# create more complex input values
fit_linear2 <- function(p=NULL){ 
  if(is.null(p)){
    p <- list(x=runif(20), y=runif(20)) #create variable p with p$x and p$y if no p varaible value is specified; should be random
  }
  my_mod <- lm(p$y~p$x)
  my_out <- list(slope=summary(my_mod)$coefficients[2,1],
                 p_val=summary(my_mod)$coefficients[2,4])
  plot(x=p$x,y=p$y)
  return(my_out)
}
fit_linear2()

my_pars <- list(x=1:10, y=runif(10))
fit_linear2(p=my_pars)
fit_linear2(my_pars)

# -------------------------------------------------------------------------

# use do.call to pass a list of parameters to a function

z <- c(runif(99),NA) #99 values with NA as 100th value
mean(z)
mean(x=z,na.rm=TRUE) # removes NA from mean calculation
mean(x=z,na.rm=TRUE,trim=0.05)
my_list=list(x=z,na.rm=TRUE,trim=0.05)
mean(my_list)
do.call(mean,my_list)
