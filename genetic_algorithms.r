number_and_operators = c("10", "25", "100", "5", "3", "+", "-", "/", "*")
target = 2512
n = length(number_and_operators)
##to get the last index of numbers in vector
nums_end_index = sum(!is.na(as.integer(stringr::str_extract(number_and_operators, "\\d+"))))

randomSearch <- function() {
  time_random <- 0
  for(j in 1:5){
    start_time_ran <- Sys.time()
    end_time_ran <- Sys.time()
    repeat {
      x=replicate(n,0)
      nums = 1:nums_end_index
      for(i in 1:n){
        if(i%%2==1){
          x[i] <- sample(nums,1)
          nums[-which(nums==x[i])]
        }
        else{
          x[i] <- sample((nums_end_index+1):n,1)
        }
      }
      str = paste(number_and_operators[x], collapse="")
      x = eval(parse(text=str))
      if(abs(target-x) == 0){
        end_time_ran <- Sys.time()
        time_random = time_random + (end_time_ran - start_time_ran)
        break
      }
    }
  }
  return(time_random/10)
}


myInitPopulation <- function(object)
{
  population <- matrix(NA, nrow = object@popSize, ncol = n)
  for(j in 1:object@popSize){
    numbers = 1:nums_end_index
    operators = (nums_end_index+1):n
    p=replicate(n,0)
    for(i in 1:n){
      if(i%%2 == 1){
        temp <- sample(numbers,1)
        if(length(numbers) == 1)p[i]=numbers
        else p[i] <- temp
        numbers <- numbers[-which(numbers==temp)]
        
      }else{
        temp <- sample(operators,1)
        p[i] <- temp
      }
    }
    population[j,] = p
  }
  population
}

fitness <-function(object)
{
  str = paste(number_and_operators[object], collapse="")
  x = eval(parse(text=str))
  -abs(target-x)
  
}

crossover <- function(object, parents)
{
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  #
  cxPoints <- sample(seq(2,n-1), size = 1)
  cxPoints <- seq(0, cxPoints)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  children[,cxPoints] <- parents[,cxPoints]
  #
  for(j in 1:2)
  { 
    diff = setdiff(parents[-j,], children[j, cxPoints])
    nums = diff[diff <= nums_end_index]
    nums_random_options <- setdiff(setdiff(1:nums_end_index, children[j, children[j] <= nums_end_index]), nums)
    ops = diff[diff > nums_end_index]
    for(i in max(cxPoints+1):n){
      if(i%%2 == 1){
        if(length(nums) == 0)
        {
          children[j, i] =  nums_random_options[1]
          nums_random_options <- nums_random_options[-1]
        }else{
          children[j, i] = nums[1]
          nums <- nums[-1]  
        }
      }
      else {
        if(length(ops) == 0)
        {
          children[j, i] =  sample((nums_end_index+1):n, 1)
        }else{
          children[j, i] = ops[1]
          ops <- ops[-1]  
        }
      }
    }
  }
  #
  out <- list(children = children, fitness = rep(NA,2))
  return(out)
}

mutation <- function(object, parent)
{
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  repeat{
    m <- sample(1:n, size = 2)
    if(m[1]%%2 == m[2]%%2)
      break
  }
  mutate[m[1]] <- parent[m[2]]
  mutate[m[2]] <- parent[m[1]]
  return(mutate)
}

# steviloMeritev <- 4
# GA_times <- vector()
# random_times <- vector()
# 
# 
# for(i in 1:steviloMeritev){
#   print(i)
#   if(i == 1)number_and_operators=c("10", "25", "100", "5", "3", "+", "-", "/", "*")
#   if(i == 2)number_and_operators=c("10", "25", "100", "5", "3","6", "2", "+", "-", "/", "*")
#   if(i == 3)number_and_operators=c("10", "25", "100", "5", "3","6", "7","4","11",  "+", "-", "/", "*")
#   if(i == 4)number_and_operators=c("10", "25", "100", "5", "3","6", "7", "8","2","4","11", "+", "-", "/", "*")
#   target = 2512
#   n = length(number_and_operators)
#   nums_end_index = sum(!is.na(as.integer(stringr::str_extract(number_and_operators, "\\d+"))))
#   start_time_GA <- Sys.time()
#   GA <- ga(type = "permutation", fitness = fitness, lower = replicate(n,1),
#            upper = replicate(n,n),
#            popSize = 100, maxiter = 100, run = 50, population = myInitPopulation,
#            crossover = crossover, mutation = mutation, selection = ga_tourSelection)
#   end_time <- Sys.time()
#   GA_time <- end_time - start_time_GA
#   GA_times <- c(GA_times, GA_time)
#   random_times <- c(random_times, randomSearch())
# }
# 
# ggplot(df, aes(x))+
#   ggtitle("Time comparisson of random search and GA")+
#   labs(y="Time", x="Amount of available numbers")+
#   geom_line(aes(y=random_times), color="RandomSearch")+
#   geom_line(aes(y=GA_times), color="GA")+
#   scale_colour_manual("", values = c("RandomSearch"="red", "GA"="red"))
############################################################################################

GA <- ga(type = "permutation", fitness = fitness, lower = replicate(n,1), 
          upper = replicate(n,n), 
          popSize = 200, maxiter = 100, run = 100, population = myInitPopulation,
          crossover = crossover, mutation = mutation, selection = ga_tourSelection)
summary(GA)
plot(GA)

GA2 <- ga(type = "permutation", fitness = fitness, lower = replicate(n,1), 
          upper = replicate(n,n), 
          popSize = 10, maxiter = 50, run = 50, population = myInitPopulation,
          crossover = crossover, mutation = mutation, selection = ga_tourSelection)
summary(GA2)
plot(GA2)

GA3 <- ga(type = "permutation", fitness = fitness, lower = replicate(n,1), 
          upper = replicate(n,n), 
          popSize = 300, maxiter = 100, run = 500, population = myInitPopulation,
          crossover = crossover, mutation = mutation, selection = ga_tourSelection)
summary(GA3)
plot(GA3)




