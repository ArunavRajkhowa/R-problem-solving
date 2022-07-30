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