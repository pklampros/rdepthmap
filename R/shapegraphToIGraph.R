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
  linksunlinks = rdepthmap::getShapeGraphLinksUnlinks(graphFile)
  links = linksunlinks[linksunlinks$link == 1,]
  links = links[,c("refA","refB")]
  unlinks = linksunlinks[linksunlinks$link == 0,]
  unlinks = unlinks[,c("refA","refB")]
  connections = rdepthmap::getShapeGraphConnections(graphFile)
  if (nrow(connections) ==0 ) {
    edges = links
  } else {
    edges = connections
  }

  for (i in 1:nrow(edges)){
    edges[i, ] = sort(edges[i,c("refA","refB")])
  }
  edges = edges[!duplicated(edges),]

  ogr@data$x = as.data.frame(gCentroid(ogr, byid = T))[,1]
  ogr@data$y = as.data.frame(gCentroid(ogr, byid = T))[,2]

  refA = edges$refA
  refB = edges$refB
  Depth_Ref = ogr@data$Depthmap_Ref
  ogr@data  = ogr@data[,c("Depthmap_Ref",names(ogr@data)[names(ogr@data) != "Depthmap_Ref"])]
  if (!is.na(weightcolumn)) {
    edges$weight = ((ogr@data[match(refA, Depth_Ref), weightcolumn])+(ogr@data[match(refB, Depth_Ref), weightcolumn]))/2
    graph = graph.data.frame(edges, directed = FALSE, vertices = ogr@data)
    E(graph)$weight = edges$weight
  } else {
    graph = graph.data.frame(edges, directed = FALSE, vertices = ogr@data)
  }
  return(graph);
}
