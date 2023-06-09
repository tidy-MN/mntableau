% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publish_data_frame.R
\name{publish_data_frame}
\alias{publish_data_frame}
\title{Publish a R data frame as a Tableau published data source}
\usage{
publish_data_frame(
  data,
  conn,
  name,
  project_name,
  parent_project_name = NULL,
  datasource_description = NULL
)
}
\arguments{
\item{data}{A data frame that can be published as a data source.}

\item{conn}{A Tableau connection object. See `tableau_connect()`.}

\item{name}{The name of the published data source.}

\item{project_name}{The name of the project to publish to (must exist already). Case sensitive.}

\item{parent_project_name}{The name of the parent project of `project_name`. Use if `project_name` is not a unique project name on your Tableau site. Optional.}

\item{datasource_description}{A string describing the published data source. Optional.}
}
\description{
Requires reticulate and Python interpreter
}
\examples{
/dontrun{

keyring::key_set("tableau", "nageld1")

conn <- tableau_connect("mdh_internal_dev",
username = "nageld1",
password = keyring::key_get("tableau", "nageld1")
)

publish_data_frame(tableau_sites, conn,
name = "MN Tableau sites",
project_name = "published_data_sources",
parent_project_name = "tableau_api_lib testing",
datasource_description = "MN state government Tableau site info"
)
}

}
