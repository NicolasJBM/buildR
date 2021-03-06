#' @name net_make_dynamic
#' @title Make a dynamic network
#' @author Nicolas Mangin
#' @description Make a dynamic network from a list of graphs
#' @param graphlist   Tibble. One column must contain the "period" and another should contain the "graph"
#' @param periodvar   Character. Name of the variable containing the period.
#' @param graphvar    Character. Name of the variable containing the graph.
#' @param node_weight Character. Name of the variable containing the weights of the nodes in each graph.
#' @param node_com    Character. Name of the variable containing the community of the nodes in each graph.
#' @param edge_weight Character. Name of the variable containing  the weights of the edges in each graph.
#' @param lscale      Numeric. Scaling factor for labels.
#' @param vscale      Numeric. Scaling factor for vertices.
#' @param escale      Numeric. Scaling factor for edges.
#' @param colors      Character vector. Colors of the communities.
#' @return A D3 movie showing the evolution of the network.
#' @importFrom tidygraph as.igraph
#' @importFrom tidygraph activate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom intergraph asNetwork
#' @importFrom networkDynamic networkDynamic
#' @importFrom ndtv compute.animation
#' @importFrom ndtv render.d3movie
#' @importFrom network %v%
#' @importFrom network %e%
#' @importFrom grDevices palette
#' @export


net_make_dynamic <- function(
                             graphlist,
                             periodvar = "period",
                             graphvar = "graph",
                             node_weight = NULL,
                             node_com = NULL,
                             edge_weight = NULL,
                             lscale = 0.05,
                             vscale = 0.05,
                             escale = 0.05,
                             colors = c(
                               "red", "blue", "green", "orange",
                               "purple", "darkgreen", "darkred"
                             )) {
  stopifnot(
    periodvar %in% names(graphlist),
    graphvar %in% names(graphlist),
    !is.null(node_weight),
    !is.null(node_com),
    !is.null(edge_weight)
  )

  graph <- NULL
  name <- NULL
  label <- NULL
  community <- NULL
  edges <- NULL
  from <- NULL
  to <- NULL

  dyngraph <- graphlist %>%
    dplyr::select(period = periodvar, graph = graphvar) %>%
    dplyr::mutate(
      graph = purrr::map(
        graph,
        function(x, node_weight, node_com, edge_weight) {
          intergraph::asNetwork(
            tidygraph::as.igraph(
              x %>%
                tidygraph::activate("nodes") %>%
                dplyr::select(
                  name,
                  label,
                  weight = node_weight,
                  community = node_com
                ) %>%
                dplyr::mutate(community = as.character(community)) %>%
                tidygraph::activate(edges) %>%
                dplyr::select(from, to, weight = edge_weight)
            )
          )
        },
        node_weight = node_weight,
        node_com = node_com,
        edge_weight = edge_weight
      )
    )

  timenet <- as.list(dyngraph$graph)

  movie <- networkDynamic::networkDynamic(
    base.net = timenet[[length(timenet)]],
    network.list = timenet,
    # vertex.pid = "label",
    onsets = seq(from = 0, to = (length(timenet) - 1)),
    termini = seq(from = 1, to = length(timenet)),
    verbose = FALSE
  )

  ndtv::compute.animation(
    movie,
    animation.mode = "kamadakawai",
    slice.par = list(
      start = 0,
      end = (length(timenet) - 1),
      interval = 1,
      aggregate.dur = 1,
      rule = "any"
    ),
    verbose = FALSE
  )

  chart <- ndtv::render.d3movie(
    movie,
    usearrows = FALSE,
    displaylabels = TRUE,
    vertex.tooltip = (timenet[[length(timenet)]] %v% "label"),
    label = "label",
    bg = "#ffffff",
    vertex.border = "#333333",
    vertex.cex = (timenet[[length(timenet)]] %v% "weight") * vscale,
    vertex.col = colors[as.integer(as.factor(
      timenet[[length(timenet)]] %v% "community"
    ))],
    label.cex = lscale,
    edge.lwd = (timenet[[length(timenet)]] %e% "weight") * escale,
    edge.col = "#55555599",
    launchBrowser = TRUE,
    render.par = list(tween.frames = 30, show.time = F),
    plot.par = list(mar = c(0, 0, 0, 0)), output.mode = "html",
    verbose = FALSE
  )

  return(chart)
}
