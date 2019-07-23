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

createGrid = function(graphFileIn, graphFileOut = NA, gridSize,
                      cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "VISPREP",
             "-pg", gridSize)
  depthmapXcli(params, cliPath, verbose);
}

fillGrid = function(graphFileIn, graphFileOut = NA, fillX, fillY,
                    cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  tmpPtz = tempfile(fileext = ".tsv");
  dt = data.frame(x = fillX, y = fillY)
  write.table(dt, tmpPtz, row.names = F, quote = F, sep = "\t")

  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "VISPREP",
             "-pf", tmpPtz)

  depthmapXcli(params, cliPath, verbose);
  removed = file.remove(tmpPtz)
}

makeVGAGraph = function(graphFileIn, graphFileOut = NA, maxVisibility = NA, boundaryGraph = FALSE,
                        cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "VISPREP",
             "-pm")
  if (!is.na(maxVisibility)) params = c(params, "-pr", maxVisibility)
  if (boundaryGraph) params = c(params, "-pb")

  depthmapXcli(params, cliPath, verbose);
}

unmakeVGAGraph = function(graphFileIn, graphFileOut = NA, removeLinks = FALSE,
                      cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "VISPREP",
             "-pu")

  if (removeLinks) params = c(params, "-pl")

  depthmapXcli(params, cliPath, verbose);
}
