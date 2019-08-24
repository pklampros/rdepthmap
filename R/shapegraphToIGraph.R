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

shapegraphToIGraph = function(graphFile, weightcolumn = NA){
  ogr = rdepthmap::getShapeGraph(graphFile)
  connections = rdepthmap::getShapeGraphConnections(graphFile)
  for (i in 1:nrow(connections)){
    connections[i, ] = sort(connections[i,c("refA","refB")])
  }
  connections = connections[!duplicated(connections),]

  ogr@data$x = as.data.frame(gCentroid(ogr, byid = T))[,1]
  ogr@data$y = as.data.frame(gCentroid(ogr, byid = T))[,2]

  refA = connections$refA
  refB = connections$refB
  Depth_Ref = ogr@data$Depthmap_Ref
  ogr@data  = ogr@data[,c("Depthmap_Ref",names(ogr@data)[names(ogr@data) != "Depthmap_Ref"])]
  if (!is.na(weightcolumn)) {
    connections$weight = ((ogr@data[match(refA, Depth_Ref), weightcolumn])+(ogr@data[match(refB, Depth_Ref), weightcolumn]))/2
    graph = graph.data.frame(connections, directed = FALSE, vertices = ogr@data)
    E(graph)$weight = connections$weight
  } else {
    graph = graph.data.frame(connections, directed = FALSE, vertices = ogr@data)
  }
  return(graph);
}
