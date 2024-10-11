#' Plot (Method)
#'
#' Function used to plot an assessment object.
#'
#' @param x Assessment object to display.
#' @param bg_color Background color. The default is white.
#' @param verices_color Color of the nodes label.
#' @param ... Additional arguments affecting the summary produced.
#' @return The function returns the graph representation of the structure.
#' @export plot.assessment
#' @export
#'
#' @examples
#' #  Example: Random items presentation
#'
#' token<- assessment(N_items = 5 ,adaptive = FALSE)
#' plot(token)
plot.assessment<- function(x,bg_color=NULL,verices_color="black",...) {
  if(is.null(x$states))
  {
    stop("There is not states field in the object")
  }
  oldpar<-graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar)) #Restore default options for the graph
    K<-as.data.frame(x$states)
    likelihood<-x$likelihood
    color<- grDevices::heat.colors(100)
    color<-color[100-round(likelihood,2)*100]
    colnames(K)<-paste0("K",colnames(K))
    pref<-paste("rPref::high(",colnames(K),")",collapse = "*")
    pref<-eval(parse(text=pref))
    btg<-rPref::get_btg(K,pref, use_dot = FALSE)
    graphics::par(bg= bg_color)
    Graph<-igraph::plot.igraph(btg$graph,vertex.color=color,layout=btg$layout,vertex.size=7,edge.arrow.size=0,
                margin=0,edge.lty=1,vertex.label.color=verices_color,vertex.label.dist=1.5)

}
