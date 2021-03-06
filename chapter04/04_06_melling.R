# Chapter 4, Exercise 6
# p170
# See page 134-135

# Use the Logistic Regression Formula Y=e^x/(1+e^x)  x = b0 + b1*x1 + b2*x2

Y = e^ b0 + b1 * x1
    ----------------
    1 + e^ b0 + b1 * x1

# 6a ####

b0 = -6
b1 = .05
b2 = 1

x1 = 40
x2 = 3.5

y0 = b0 + b1 * x1 + b2 * x2
y0
y = exp(y0)/(1 + exp(y0))

y # 0.3775407 = 37.7%

# 6b ####

# Use the Logistic Regression Formula Y=e^x/(1+e^x)  x = b0 + b1*x1 + b2*x2
# Y = 0.5
# x2 = 3.5 (from a)
0.5 = e^-6 + 0.05X + 1*3.5
      ------------------------
      1 + e^-6 + 0.05X + 1*3.5

(1 + e^-6 + 0.05X + 1*3.5) * 0.5 = e^-6 + 0.05X + 1*3.5
(0.5 + 0.5 * (e^-6 + 0.05X + 1*3.5)   = e^-6 + 0.05X + 3.5
0.5 = 0.5 * e^-6 + 0.05X + 3.5
1 = e^-6 + 0.05X + 3.5
log(1) = log(e^-6 + 0.05X + 3.5)
0 = -6 + 0.05X + 3.5
0 = -2.5 + 0.05X
2.5 = 0.05X
50 = X

# Solve for x1
# Will be taking the log of both sides

log(1) # 0
x1 = 50

  