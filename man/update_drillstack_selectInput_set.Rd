% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\name{update_drillstack_selectInput_set}
\alias{update_drillstack_selectInput_set}
\title{update_drillstack_selectInput_set}
\usage{
update_drillstack_selectInput_set(session, activepage = input$activepage)
}
\arguments{
\item{session}{the session variable used in shiny::updateSelectInput which is called here}

\item{activepage}{if we are in RAD change things a little from program costing}
}
\description{
This function updates the block of selectInputs in the modal of update stack level stacking.
}
\examples{
update_drillstack_selectInput_set(session,activepage=input$activepage)
}
