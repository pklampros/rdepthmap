# Copyright 2019 Fani Kostourou
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

segmentAnalysis = function(graphFileIn, graphFileOut = NA, analysisType, radii, radiusType,
                           tulipBins = NA, weightWithColumn = NA, includeChoice = FALSE,
                           cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  if (!(analysisType %in% c("tulip", "metric", "angular", "topological"))) {
    stop(paste0("Unknown segment analysis type: ", analysisType))
  }
  if (!(radiusType %in% c("steps", "metric", "angular"))) {
    stop(paste0("Unknown radius type: ", radiusType))
  }
  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "SEGMENT",
             "-st", analysisType,
             "-sr",  paste(radii, collapse = ","),
             "-srt", radiusType)
  if (includeChoice) params = c(params, "-sic")
  if (!is.na(tulipBins)) params = c(params, "-stb", tulipBins)
  if (!is.na(weightWithColumn)) params = c(params, "-swa", formatForCLI(weightWithColumn))
  depthmapXcli(params, cliPath, verbose);
}
