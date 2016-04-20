#Chapter 4 Qs 47, 52, 57 

ch4_q47 <- function(m, s, multiplier) {
#verify the normal probability rule

a = pnorm(multiplier*s,mean=m,sd=s)
b = pnorm(multiplier*s*-1, mean=m, sd=s)
print(paste0("Probability of -", multiplier, " <= X <= ", multiplier))
print(a-b)

}

ch4_q52 <- function() {


	n = 20
	p = 0.30
	s = sqrt(n*p*(1-p))

	print("Part A: P[X <= 3]")
	a = pnorm(3, mean=n*p, sd=s)
	b = 0.1071
	c = pbinom(3, size=n, prob=p)
	print(a)
	print(b)
	print(c)
	print("")

	print("Part B: P[3 <= X <= 6]")
	d = pnorm(6, mean=n*p, sd=s)
	e = d - a
	f = 0.6080 - b
	g = pbinom(6, size=n, prob=p) - c
	print(e)
	print(f)
	print(g)
	print("")

	print("Part C: P[4 <= X]")
	h = pnorm(20, mean=n*p, sd=s) - pnorm(4, mean=n*p, sd=s)
	i = 1 - 0.2375
	j = pbinom(20, size=n, prob=p) - pbinom(4, size=n, prob=p)
	print(h)
	print(i)
	print(j)
	print("")

	print("Part D: P[4 = X]")
	k = dnorm(4, mean=n*p, sd=s)
	l = 0.2375 - 0.1071
	m = dbinom(4, size=n, prob=p)
	print(k)
	print(l)
	print(m)

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


