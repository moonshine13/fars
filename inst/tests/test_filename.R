test_that("filename", {
  filename<-make_filename(2015)
  expect_that(filename, is_a("character"))
  expect_that(filename, matches("accident_2015.csv.bz2"))
})
