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

segmentToGraph = function(graphFile, weightcolumn = "Segment Length"){
  segm.ogr = rdepthmap::getShapeGraph(graphFile)
  segm.connections = rdepthmap::getShapeGraphConnections(graphFile)
  for (i in 1:nrow(segm.connections)){
    segm.connections[i, ] = sort(segm.connections[i,c("refA","refB")])
  }
  segm.connections = segm.connections[!duplicated(segm.connections),]

  segm.ogr@data$x.coords = as.data.frame(gCentroid(segm.ogr, byid = T))[,1]
  segm.ogr@data$y.coords = as.data.frame(gCentroid(segm.ogr, byid = T))[,2]
  segm.ogr$segm.coords = as.matrix(segm.ogr@data[,col])

  refA = segm.connections$refA
  refB = segm.connections$refB
  Depth_Ref = segm.ogr@data$Ref
  segm.connections$weight = ((segm.ogr@data[match(refA, Depth_Ref), weightcolumn])+(segm.ogr@data[match(refB, Depth_Ref), weightcolumn]))/2

  segm.graph = graph.data.frame(segm.connections, directed = FALSE, vertices = segm.ogr@data$Ref)
  E(segm.graph)$weight = segm.connections$weight
  return(segm.graph);
}
