# all unique combinations of vectors a and b
# works, but so SLOWWWW

a<-c(1,2,3,4)
b<-c(1,2,3,4)
c<-expand.grid(a,b)

for (i in 1:nrow(c))
{
  c[i, ] = sort(c[i, ])
}

d<-unique(c)


50766
25776
64929
