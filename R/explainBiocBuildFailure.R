#' Attempt to explain why a certain Bioconductor package fails on the 
#' Bioc build machine(s)
#'
#' @param package Character string giving the package of interest.
#' @param biocVersion Character string giving the Bioconductor version.
#' @param buildNodes Character vector indicating the build nodes to check.
#'   Should be a subset of c("nebbiolo2", "tokay2", "machv2", "nebbiolo1",
#'   "riesling1", "palomino3", "merida1").
#'   If more than one node is specified, a package is considered to fail
#'   if it fails on at least one of them. A failure is recognized by
#'   an "ERROR" returned in the build report, in any of the stages.
#' @param showOnlyFailing Logical, whether to show all dependencies or
#'   just the failing ones (and the intermediate nodes between the failing
#'   ones and the package of interest) in the output graph.
#' @param repo Character vector indicating the repo(s) to consider. Will be
#'   passed to \code{biocPkgList()}.
#' @param dependencies Character vector indicating the types of dependencies
#'   to consider. Will be passed to \code{biocPkgList()}.
#'
#' @author Charlotte Soneson
#'
#' @return Plots a graph containing the package of interest as well as its 
#'   dependencies. For each package, the graph displays the name, the date 
#'   when it was last changed, and the fraction of its direct descendants 
#'   (packages that depend on or import it) that currently fail. Note that only 
#'   Bioc dependencies are shown.
#' 
#' @importFrom BiocPkgTools biocBuildReport biocPkgList 
#'   buildPkgDependencyDataFrame buildPkgDependencyIgraph
#' @importFrom dplyr group_by summarize filter pull left_join mutate '%>%'
#' @importFrom igraph induced_subgraph V set_vertex_attr
#' @importFrom visNetwork toVisNetworkData visEdges
#' @importFrom BiocManager version
#' @importFrom rlang .data
#' 
#' @export
#' 
#' @examples
#' explainBiocBuildFailure("Rhisat2")
#' 
explainBiocBuildFailure <- function(package,
                                    biocVersion = BiocManager::version(),
                                    buildNodes = c("nebbiolo2", "tokay2", 
                                                   "machv2", "nebbiolo1",
                                                   "riesling1", "palomino3",
                                                   "merida1"),
                                    showOnlyFailing = FALSE,
                                    repo = "BioCsoft",
                                    dependencies = c("Depends", "Imports")) {

    ## Get build report, possibly subset to specific platform(s) and
    ## summarize with a single pass/fail for each package
    biocVersion <- as.character(biocVersion)
    # Prevent notes on checks
    br <- pkg <- node <- result <- last_changed_date <- NULL
    fail <- frac_fail <- Package <- Version <- dependsOnMe <- importsMe <- NULL
    br <- BiocPkgTools::biocBuildReport(version = biocVersion) %>%
        dplyr::filter(node %in% buildNodes) %>%
        dplyr::group_by(pkg) %>%
        dplyr::summarize(fail = ("ERROR" %in% result),
                         last_changed = max(.data$git_last_commit_date),
                         .groups = "drop")

    ## Get list of packages and build dependency graph
    bpl <- BiocPkgTools::biocPkgList(version = biocVersion, repo = repo)
    pddf <- BiocPkgTools::buildPkgDependencyDataFrame(dependencies = dependencies,
                                                     version = biocVersion,
                                                     repo = repo)
    pdg <- BiocPkgTools::buildPkgDependencyIgraph(pddf)

    ## Include only Bioc packages (we don't have build status for others)
    pdg <- igraph::induced_subgraph(pdg, unique(pddf$Package))

    ## Get all dependencies reachable from the package of interest
    deppkgs <- igraph::subcomponent(pdg, package, mode = "out")

    ## Get the induced subgraph of dependencies for 'package'
    pdg.pkg <- igraph::induced_subgraph(pdg, deppkgs)
    v.pkg <- igraph::V(pdg.pkg)

    ## Add information for vertices
    ## 1. Whether it fails or not
    ## 2. When it was last updated
    ## 3. What fraction of its descendants that fail
    get_frac_downstream_fails <- function(l1, l2, buildrep) {
        pkgs <- unique(unlist(c(l1, l2)))
        buildrep %>% dplyr::filter(pkg %in% pkgs) %>%
            dplyr::summarize(frac_fail = mean(fail)) %>%
            dplyr::pull(frac_fail)
    }
    # browser()
    bpl2 <- bpl %>%
        dplyr::select(Package, Version, dependsOnMe, importsMe) %>%
        dplyr::left_join(br, by = c("Package" = "pkg")) %>%
        dplyr::filter(Package %in% names(v.pkg)) %>%
        dplyr::mutate(
            frac_downstream_fails = purrr::pmap_dbl(
                list(dependsOnMe, importsMe),
                get_frac_downstream_fails,
                buildrep = br))

    pdg.pkg <- igraph::set_vertex_attr(
        pdg.pkg, name = "version", index = v.pkg,
        value = bpl2[match(names(v.pkg), bpl2$Package), "Version", drop = TRUE])
    pdg.pkg <- igraph::set_vertex_attr(
        pdg.pkg, name = "fails", index = v.pkg,
        value = bpl2[match(names(v.pkg), bpl2$Package), "fail", drop = TRUE])
    v.pkg <- igraph::V(pdg.pkg)
    pdg.pkg <- igraph::set_vertex_attr(
        pdg.pkg, name = "last_changed", index = v.pkg,
        value = as.character(
            bpl2[match(names(v.pkg), bpl2$Package), "last_changed", drop = TRUE]))
    pdg.pkg <- igraph::set_vertex_attr(
        pdg.pkg, name = "frac_downstream_fails", index = v.pkg,
        value = bpl2[match(names(v.pkg), bpl2$Package), "frac_downstream_fails", drop = TRUE])
    v.pkg <- igraph::V(pdg.pkg)

    ## Subset to only nodes that fail
    if (showOnlyFailing) {
        pdg.pkg <- igraph::induced_subgraph(
            pdg.pkg,
            unique(names(unlist(igraph::all_simple_paths(
                pdg.pkg, from = package,
                to = names(v.pkg)[igraph::get.vertex.attribute(pdg.pkg, "fails")])
            )))
        )
    }

    data <- visNetwork::toVisNetworkData(pdg.pkg)
    if (nrow(data$edges) > 0) {
        data$edges$color <- "black"
    }
    data$nodes$color <- "green"
    data$nodes$color[data$nodes$fails] <- "red"
    data$nodes$label <- paste0(data$nodes$id, "\n",
                               data$nodes$last_changed, "\n",
                               signif(data$nodes$frac_downstream_fails, digits = 3))
    visNetwork::visNetwork(nodes = data$nodes, edges = data$edges) %>%
        visNetwork::visEdges(arrows = "from") %>%
        print()
    invisible(data$nodes)
}
