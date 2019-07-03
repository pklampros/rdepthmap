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

axialAnalysis = function(graphFileIn, graphFileOut = NA, radii, includeChoice = FALSE,
                         includeLocal = FALSE, includeIntermediateMetrics = FALSE,
                         cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "AXIAL",
             "-xa", paste(radii, collapse = ","))
  if (includeChoice) params = c(params, "-xac")
  if (includeLocal) params = c(params, "-xal")
  if (includeIntermediateMetrics) params = c(params, "-xar")
  depthmapXcli(params, cliPath, verbose);
}
