% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_followPath.R
\name{followPath}
\alias{followPath}
\title{followPath}
\usage{
followPath(tree, path, sep = "/", root = "root")
}
\arguments{
\item{tree}{explored hierarchy list thanks to path}

\item{path}{vector of characters matching tree's nodes' names and
separated with sep.}

\item{sep}{separators between path elements (aka tree names)}

\item{root}{if your path has a root name for root node, enter its name here.
Else, enter NULL.}
}
\description{
Takes a hierarchy list (tree), a path written in a vector pasted
with sep, and returns the leaf.
}
