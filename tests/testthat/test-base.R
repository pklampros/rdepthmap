# Copyright 2019 Petros Koutsolampros
#
# This file is part of rdepthmap
#
# rdepthmap is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# rdepthmap is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rdepthmap  If not, see <https://www.gnu.org/licenses/>.

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
