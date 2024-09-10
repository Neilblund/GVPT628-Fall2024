# Quick illustration of a slow R loop: 

# install.packages(microbenchmark)
library(microbenchmark) # package for analyzing how long code takes to run


# This is just a function that sums a vector using a loop 
# (the sum() function does the same thing)
sumFunction<-function(x){
  total<-0
  for(i in x){
    total <- total+i
  }
  return(total)
}




set.seed(100)
vec<-rnorm(10000) # draw 10000 random numbers

sum(vec)               # same results!
sumFunction(vec)       # 

microbenchmark(sum(vec), sumFunction(vec)) # ...but the loop is much slower


# Many basic R commands like sum() actually also rely on loops, but the loops
# are written in C instead of being written in R. These functions will almost
# always be much more efficient than an equivalent R loop because they have far
# less overhead when sending stuff to your computer's processor compared to R
# (the same things that make R easier to use than a language like C also make it
# slower, alas)

# It can be a little hard to tell which functions this applies to, but as a rule:
# if you can avoid making a loop, you should avoid doing so! 

# See here for a more detailed discussion of this and other speed related issues
# in R: http://adv-r.had.co.nz/Performance.html
