#Chapter 4 Qs 47, 52, 57 

ch4_q47 <- function(m, s, multiplier) {
#verify the normal probability rule

a = pnorm(multiplier*s,mean=m,sd=s)
b = pnorm(multiplier*s*-1, mean=m, sd=s)
print(paste0("Probability of -", multiplier, " <= X <= ", multiplier))
print(a-b)

}

ch4_q52 <- function() {

n=20
p=0.30

print("A: P[X <= 3]")
a = pbinom(3, size=n, prob=p)
print(a)

print("B: P[3 <= X <= 6")
b = pbinom(6, size=n, prob=p) - a
print(b)

print("C: P[4 <= X]")
c = pbinom(20, size=n, prob=p) - pbinom(4, size=n, prob=p)
print(c)

print("D: P[4 = X]")
d = dbinom(4, size=n, prob=p)
print(d)

}

ch4_q57 <- function() {

l = (1*60*60)/40

print("Average number of planes arriving or departing at O'Hare")
print("1 every 40 seconds or 1/40 per second")
print("Average number of planes per hour (3600*per second rate)")
print(l)
print("Probability of 75 or more flights in a random hour")
print(ppois(74, lambda=l, lower=FALSE))
print("Probability of less than 100 flights in a random hour")
print(ppois(99, lambda=l))

}

#main
print('Chapter 4 Question 47')
m=0
s=1
print(paste0("Mean: ", m))
print(paste0("Standard Deviation: ", s))
ch4_q47(m, s, 1)
ch4_q47(m, s, 2)
ch4_q47(m, s, 3)
print("")
print("Chapter 4 Question 52")
ch4_q52()
print("")
print("Chapter 4 Question 57")
ch4_q57()
