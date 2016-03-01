#Chapter 7: 39, 41, 43, 45, 47, 49, 61


ch7q47 <- function() {

	x = c(0.645, 0.654, 0.640, 0.627, 0.626, 0.649, 0.629, 0.631, 0.643, 0.633, 0.646, 0.630, 0.634, 0.631, 0.651, 0.659, 0.638, 0.645, 0.655, 0.624, 0.658, 0.658, 0.658, 0.647, 0.665)

	xbar = sum(x) / length(x)

	print("n")
	print(length(x))
	print("Sample mean")
	print(xbar)

	print("Part B")
	print("alpha, ci")
	alpha = 0.05
	c = 1 - alpha
	print(alpha)
	print(c)
	print("Z alpha/2)")
	zalpha2 = qnorm((1 - alpha/2))
	print(zalpha2)
	print("xbar +-")
	l1 = (zalpha2*0.01)/5
	print(xbar)
	print("+-")
	print(l1)


	print("90% CI")
	print(xbar)
	print("+-")
	alpha2 = 0.10
	zalpha22 =  qnorm((1 - alpha2/2))
	l2 = (zalpha22*0.01)/5
	print(l2)


	print("99% CI")
	print(xbar)
	print("+-")
	alpha3 = 0.01
	zalpha23 =  qnorm((1 - alpha3/2))
	l3 = (zalpha23*0.01)/5
	print(l3)
}

