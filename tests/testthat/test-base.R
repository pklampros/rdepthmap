context("Base tests")
library(sp)

test_that("proper formatForCLI output", {
  expect_equal(formatForCLI("A map"), "\"A map\"")
})

test_that("export", {
  graphFileIn = system.file("extdata", "gallery_connected.graph", package = "rdepthmap")
  testOutputMap = system.file("extdata", "gallery_connected_vga.csv", package = "rdepthmap")
  testOutputLinks = system.file("extdata", "gallery_connected_links.csv", package = "rdepthmap")

  expect_equal(processPointMap(testOutputMap, sep = ","), getPointmapData(graphFileIn))
  expect_equal(read.csv(testOutputLinks), getPointmapLinks(graphFileIn))
  expect_equal(processPointMapAndLinks(testOutputMap, testOutputLinks, sep = ","),
               getPointmapDataAndLinks(graphFileIn));
})
