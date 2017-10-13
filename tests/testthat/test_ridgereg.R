context("ridgereg")

data(iris)
test_that("ridgereg has same coefficients as lm.ridge", {
  lmridge <- MASS::lm.ridge(
    formula = Petal.Length ~ Sepal.Width + Sepal.Length,
    data = iris,
    lambda = 0.5
  )

  ourridge <- ridgereg(
    formula = Petal.Length ~ Sepal.Width + Sepal.Length,
    data = iris,
    lambda = 0.5
  )

  # Truncate since rounding is a shit primitive
  expect_equal(trunc(ourridge$coef() * 10^2) / 10^2, trunc(lmridge$coef * 10^2) / 10^2)
})
