% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_URL_Input.R
\name{URL_Input_UI}
\alias{URL_Input_UI}
\alias{URL_Input}
\title{URL Input}
\usage{
URL_Input_UI(id, label = "URL", width = "100\%")

URL_Input(id)
}
\arguments{
\item{id}{(character) shiny module inputId.}

\item{label}{(character) display label for the control, or NULL for no label.}

\item{width}{(character) the width of the input, e.g. '400px', or '100\%'; see
validateCssUnit().}
}
\value{
(output of calling the module part)
If input is a valid URL (regex-tested + curl-tested), returns input.
Else, returns NA.
}
\description{
URL Shiny input module. Implements elementary test to get a valid URL.
}
\section{Functions}{
\itemize{
\item \code{URL_Input}: 
}}

