#' @export
#' @import igraph
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics grconvertX
#' @importFrom graphics grconvertY
#' @importFrom graphics plot
#' @param nodeLogic This is the connections between the nodes.
#' @param wd This is the working directory to save the HTML source code in. If not given, the file will be saved in the default working directory.
#' @param names This allows you to put in your own names in the nodes when generating the maze.
#' @description This function generates an network Maze with at most 2 arrows.
#' @details This function creates a maze and is saved into your working directory. At most up to 2 arrows per maze is generated.
#' @author Aiden Loe
#' @title Generate Network Maze (No arrows)
#' @examples
#'
#'#create node logic
#'logic <- nodeLogic(value = 8, type= "circuit", itemFamily= 1)
#'
#' #Folder to save html/
#' #setwd("~/desktop")
#' #filePath<- getwd()
#'
#' #Generate item
#' set.seed(1)
#' netHTML(logic, wd=NULL, names=NULL)
#'
#'


netHTML <- function(nodeLogic= NULL, wd = NULL, names=NULL){
  if(is.null(nodeLogic)){
    warnings("Please insert nodeLogic.")
  }

  if(class(nodeLogic)!="igraph"){
    stop("please use nodeLogic function")
  }

  if(is.null(wd)){
    message("HTML file is saved in default working directory.")
  }
##### html ####
if(is.null(wd)){
  wd = getwd()
}

htmlfile = file.path(paste0(wd, "/maze.html"))
cat("\n<html><head>",file=htmlfile)
button<- css()
cat(button, append=TRUE, file=htmlfile)

cat("\n<html></head>", append=TRUE, file = htmlfile)
cat("\n<br>", append=TRUE, file = htmlfile)
cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:20px;color:#FFF\">Level 1</p>",append=TRUE, file = htmlfile)
cat("\n<body>", append = TRUE, file = htmlfile)
cat("<script src='script.js'></script>",append=TRUE, file=htmlfile)
cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:14px;\"><font color=\"white\">To solve the puzzle, travel on every path. You can return to the same node but you can only use each path once.</font></p>", append=TRUE, file=htmlfile)
cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:14px;\"><font color=\"white\">Click on any node to begin.</font></p>", append=TRUE, file=htmlfile)





####### Create Node coordinates
o <- suppressWarnings(logicMap(nodeLogic ,base.colour=3, start.colour=9,end.colour= 4,names=names,newValue=9,default.colour=FALSE, no.label=FALSE))
o

#Must normalise coordinates
#grconvertX and gconvertY create the right pixel coordinates.
coordinates <- layout_with_dh(o)
coordinates.1 <- layout.norm(coordinates)

# Plot Graph
#save empty .png
png(filename="map.png", height=1000, width=1000)
#plot graph, png must always be forced.
plot.igraph(o,
     layout=layout_with_dh,
     vertex.shape='square',
     vertex.size=10,
     vertex.label.cex=0.5) # plot using desired coordinates)


# combind vector of coordinates
coord. <- cbind(grconvertX(coordinates.1[, 1], "user", "device"), cbind(grconvertY(coordinates.1[, 2], "user", "device")))
coord.1 <- apply(coord., 1:2, function(x) x/1.8) #adjust node coordinates
dev.off()

cat("\n<div align=center>", append = TRUE, file=htmlfile)
cat("<div class=box>", append=TRUE, file=htmlfile)

#margin:0 auto puts container in the center when it is a fixed width.
cat("\n<div style= 'position:relative;width:auto; height:auto;margin:0 auto' id = 'graphContainer'>", append=TRUE, file=htmlfile)  #position:relative(parent)

#Position of the buttons is relative to the div position
#z index puts the div tag at the top, so the nodes are above the edges.
n.name <-  unlist(V(o)$name)
buttons = ""
for (j in 1:nrow(coord.)){
  buttons <- paste0(buttons,"\n<div onClick='nodeClick(this)' id = '", j,"'", "; class = 'myButton'; style = 'z-index:1; left:",(coord.1[j,1]),"px;top:",(coord.1[j,2]),"px'>",n.name[j],"</div>")
}
cat(buttons, append=TRUE, file=htmlfile)
buttons
ed<- ends(o, E(o), names=FALSE)


##### Extract start node coordinates.
#Number of rows we need from vector.
start.index <- ed[,1]

#Number of columns we need from matrix.
start.coord <- ncol(coord.)

#Empty Matrix
start.node.coord <- matrix(NA, nrow = length(start.index), ncol = start.coord)
for(i in 1:length(start.index)){
  start.node.coord[i,]<- coord.[start.index[i],]
}
start.node.coord.1 <- apply(start.node.coord,1:2, function(x) x/1.8)



##### Extract end node coordinates.
end.index<- ed[,2]
end.coord <- ncol(coord.)

#Create empty matrix
end.node.coord <- matrix(NA, nrow = length(end.index), ncol= end.coord)
for(i in 1:length(end.index)){
  end.node.coord[i,] <- coord.[end.index[i],]
}
end.node.coord.1 <- apply(end.node.coord,1:2, function(x) x/1.8) #adjust edge coord


#### Edges of Nodes
connections = ""
for (i in 1:nrow(ed)){
  connections <- paste0(connections," \n linedraw(",start.node.coord.1[i,1],',',start.node.coord.1[i,2],',',end.node.coord.1[i,1],',',end.node.coord.1[i,2],',',paste0('"',ed[i,1],'_',ed[i,2],'"'),")")
}

cat("\n<div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n<div id=\"hidden\">&nbsp;</div>", append=TRUE, file=htmlfile) #saves all the info into the id=hidden. concerto will automatically save all this info in a form.
cat("\n<div id=\"hidden2\">&nbsp;</div>", append=TRUE, file=htmlfile)
cat("\n<input name=\"next\" style=\"display: none;\" type=\"Submit\" value=\"next\" />",append=TRUE, file=htmlfile) # For concerto
#cat("\n<input type = 'submit' value = 'next' style='display: none'> ", append=TRUE, file = htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n<p style =\"width:150px; text-align: center; height:20px; background-color:#fff; border: 1px solid #999\" id=\"output\" hidden></p>", append=TRUE, file=htmlfile)





#  Java Script
## Draw lines without using html5 method (canvas)
drawLines <- paste0("
function linedraw(x1, y1, x2, y2, lineid){
    if(y1 < y2){
    var pom = y1;
    y1 = y2;
    y2 = pom;
    pom = x1;
    x1 = x2;
    x2 = pom;
    }
    var length=Math.sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));

    var a = Math.abs(x1-x2);
    var b = Math.abs(y1-y2);
    var c;
    var sx = (x1+x2)/2 ;
    var sy = (y1+y2)/2 ;
    var width = Math.sqrt(a*a + b*b ) ;
    var x = sx - width/2;
    var y = sy;

    a = width / 2;

    c = Math.abs(sx-x);

    b = Math.sqrt(Math.abs(x1-x)*Math.abs(x1-x)+Math.abs(y1-y)*Math.abs(y1-y) );

    var cosb = (b*b - a*a - c*c) / (2*a*c);
    var rad = Math.acos(cosb);
    var deg = (rad*180)/Math.PI

    htmlns = \"http://www.w3.org/1999/xhtml\";
    div = document.createElementNS(htmlns, \"div\");
    div.setAttribute('id', lineid);
    div.setAttribute('style','border:2px solid black;width:'+width+'px;height:\" + length + \"px;-moz-transform:rotate('+deg+'deg);-webkit-transform:rotate('+deg+'deg);position:absolute;top:'+y+'px;left:'+x+'px;');

    document.getElementById(\"graphContainer\").appendChild(div);

    } \n

                    ")


cat('<script>', append = TRUE, file = htmlfile)
cat(drawLines, append = TRUE, file = htmlfile)

# Step 1 Break into col vectors
start.node <- ed[,1]
start.node<- cbind(start.node)

end.node<- ed[,2]
end.node <- cbind(end.node)


travelled = 0

##### Step 2 for loop across number of col vectors

  edge.list <- "\n var edgeArray=["

for (i in 1:length(start.node)){
  if (i != 1) {
    edge.list <- paste0(edge.list,",[",start.node[i,1],",",end.node[i,1],",", travelled[],"]")
  } else {
    edge.list <- paste0(edge.list,"[",start.node[i,1],",",end.node[i,1],",", travelled[],"]")
  }
}
edge.list <- paste0(edge.list,"];")
edge.list

cat(edge.list, append=TRUE, file=htmlfile)

jsDrawLines<- jsDrawLines()
cat(jsDrawLines, append = TRUE, file = htmlfile)

cat(connections, append=TRUE, file=htmlfile)

cat("\n</script>", append = TRUE, file = htmlfile)
cat("\n</body>", append = TRUE, file = htmlfile)
cat("\n</html>", append = TRUE, file = htmlfile)
}
