library(testthat)
library(icager)

context("dummy")

test_that("dummy_corner", {
  expect_equal(length(unique(dx$corner)), 4)})
