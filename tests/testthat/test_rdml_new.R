context("Creation RDML object from different file types or user input")

PATH <- path.package("RDML")

test_that("RDML can be created from BioRad .rdml", {
  filename <- paste0(PATH, "/extdata/", "BioRad_qPCR_melt.rdml")
  rdml <- RDML$new(filename)
  expect_class(rdml, "RDML")
})

test_that("RDML can be created from StepOne .rdml", {
  filename <- paste0(PATH, "/extdata/", "stepone_std.rdml")
  rdml <- RDML$new(filename)
  expect_class(rdml, "RDML")
})

test_that("RDML can be created from LightCycler .rdml", {
  filename <- paste0(PATH, "/extdata/", "lc96_bACTXY.rdml")
  rdml <- RDML$new(filename)
  expect_class(rdml, "RDML")
})