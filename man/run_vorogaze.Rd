\name{run_vorogaze}
\alias{run_vorogaze}
\title{Run the VoroGaze Research Workbench}
\usage{
run_vorogaze(
  host = "127.0.0.1",
  port = getOption("shiny.port", 3838L),
  launch.browser = interactive()
)
}
\arguments{
\item{host}{Character scalar passed to \code{shiny::runApp()}.}
\item{port}{Integer TCP port passed to \code{shiny::runApp()}.}
\item{launch.browser}{Whether to open the app in a browser.}
}
\value{This function does not return until the Shiny application stops.}
\description{
Starts the complete workbench as a local Shiny application. By default it is
available only from the computer on which it is running.
}
