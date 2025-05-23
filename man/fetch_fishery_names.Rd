% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_public_helpers.R
\name{fetch_fishery_names}
\alias{fetch_fishery_names}
\title{List all available 'fishery name' values}
\usage{
fetch_fishery_names(...)
}
\arguments{
\item{...}{.}
}
\value{
Character vector of "fishery_name" values which represent identifiers for a given dataset, which is comprised of multiple elements (e.g., effort, interview, catch, etc.).
}
\description{
List all available 'fishery name' values
}
\examples{
head(fetch_fishery_names(), n = 10)
}
\seealso{
Other public_data: 
\code{\link{fetch_dwg}()},
\code{\link{get_fishery_data}()},
\code{\link{query_creel_website}()},
\code{\link{search_fishery_name}()}
}
\concept{public_data}
