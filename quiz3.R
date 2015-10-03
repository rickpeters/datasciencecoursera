# scripts for quiz 3

# Q1
sapply(s, function(x) colMeans(x[, c("Sepal.Length", "Sepal.Width")]))
#              setosa versicolor virginica
# Sepal.Length  5.006      5.936     6.588
# Sepal.Width   3.428      2.770     2.974

# Q2
apply(iris[, 1:4], 2, mean)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 5.843333     3.057333     3.758000     1.199333 

# Q3
with(mtcars, tapply(mpg, cyl, mean))
# 4        6        8 
# 26.66364 19.74286 15.10000 
# this is the same as:
tapply(mtcars$mpg, mtcars$cyl, mean)

# Q4
with(mtcars, tapply(hp, cyl, mean))
# 4         6         8 
# 82.63636 122.28571 209.21429 
209.21429 - 82.63636
#[1] 126.5779
