#Problem 1
#Use various utility functions discussed to create vectors containing these values. You need to create both
#[first containing 10 values and another containing 26] vectors separately.
## [1] 2 4 8 16 32 64 128 256 512 1024
## [1] "a26" "b25" "c24" "d23" "e22" "f21" "g20" "h19" "i18" "j17" "k16"
## [12] "l15" "m14" "n13" "o12" "p11" "q10" "r9" "s8" "t7" "u6" "v5"
## [23] "w4" "x3" "y2" "z1"

a=as.character(2**c(1:10))
a

b=letters
c=seq(sum(nchar(b)),1,-1)

paste0(b,c)





#Problem 2
#Below given is a vector containing first lines of many addresses. Extract city names from all the addresses
#using string functions. Hint: you can use a for loop to iterate over results of strsplit for further processing.

address_list=c("802/hiranandani/Mumbai",
               "2A/kalka-Delhi",
               "345#near adyar#Chennai",
               "10-shyaam bazzar-Kolkata")
#Result will be as follows:
## [1] "Mumbai"
## [1] "Delhi"
## [1] "Chennai"
## [1] "Kolkata"

v=c('/','-','#','-')
split = strsplit(address_list,v)
print(split)
for (i in 1:4){
  out=split[[i]][length(split[[i]])]
  print(out)
}

#another way
#temp=gsub("-","/",address_list)
#temp=gsub("#","/",temp)
#l=strsplit(temp,"/")
#for(i in 1:4){
#  print(l[[i]][3])
#}






#Problem 3
#Use following bit to create a vector with prime numbers in 1:47. [Prime numbers are numbers which are
#divisible only by themselves.]
#primes=c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47)
#Any number from 48 to 100 which is not divisible by any of the above listed primes is also a prime. Print
# prime numbers from 48 to 100.
#Result will be as follows:
  ## [1] 53
  ## [1] 59
  ## [1] 61
  ## [1] 67
  ## [1] 71
  ## [1] 73
  ## [1] 79
  ## [1] 83
  ## [1] 89
  ## [1] 97
#solution

primes=c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47) #given