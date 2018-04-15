d <- data.frame(list(kids=c("Jack", "Jill"), ages=c(12,10)))
d

kids<-c("Jack", "Jill")
ages<-c(12,10)
d <- data.frame(kids, ages, stringsAsFactors = FALSE)
d

d[[1]]
d$kids
d[,1]
str(d)