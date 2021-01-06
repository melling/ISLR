# Chapter 4 Question 12
# p172

# 12a ####

# Prints 2^3
Power = function() {
  # 2^3
  x = 2
  a = 3
  total = 1
  for (i in 1:a) {
    total = total * x
  }
  
  print(total)
}
Power()


# 12b ####

Power2 = function(x, a) {
  
  total = 1
  for (i in 1:a) {
    total = total * x
  }
  
  print(total)
}

# 12c ####

Power2(10,3); 10^3
Power2(8, 17); 8^17
Power2(131,3); 131^3



# 12d ####

Power3_old = function(x, a) {
  
  total = 1
  for (i in 1:a) {
    total = total * x
  }
  
  return(total)
}

Power3 = function(x, a) {
  result = x^a
  return(result)
}

x = Power3(2,3); x
Power3_old(1:10, 2)

# 12e: ####

x = 1:10
y = Power3(x, 2)
plot(x,y)
plot(x,y, log = "xy")

# 12f: ####


PlotPower = function(x, a) {
  plot(x, Power3(x, a), xlab = "PlotPower")
  
}

PlotPower(1:10 ,3)
