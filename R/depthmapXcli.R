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

getDefaultCLILocation = function() {
  depthmapXcli = system.file("exec", "depthmapXcli_macos", package = "rdepthmap")
  if (.Platform$OS.type == "windows") {
    depthmapXcli = system.file("exec", "depthmapXcli_win64.exe", package = "rdepthmap")
  }
  depthmapXcli
}

depthmapXcli = function(params, cliPath = getDefaultCLILocation(), verbose = FALSE) {
  suppressWarnings({
    cmdData = system2(cliPath, params, stdout = T);
  })
  if (("status" %in% names(attributes(cmdData))) && attr(cmdData,"status") != 0) {
    # errored
    for (d in cmdData) {
      if (startsWith(d, "Usage")) {
        break;
      }
      errTxt = d
    }
    stop(errTxt)
  }
  if (verbose) cat(cmdData, sep = "\n")
}

formatForCLI = function(filePath) {
  if (startsWith(filePath, "\"")) {
    return(filePath)
  }
  return(paste0("\"", filePath, "\""))
}
