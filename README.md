[![Build Status](https://travis-ci.org/JustinaXie/bis557.svg?branch=master)](https://travis-ci.org/JustinaXie/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

In the first homework, we created and documented a function that
calls `lm`. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```
In the second homework, we created and documented a function that
calls `ridge_reg`. You can use it like this:
```{R}
library(bis557)
ridge_fit <- ridge_reg(Sepal.Length ~.,lambda = 1, iris)
summary(ridge_fit)
```