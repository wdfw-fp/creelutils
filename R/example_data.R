#' Example creel survey dataset from the Skagit winter steelhead 2021 fishery
#'
#' A list containing creel survey data for the Skagit and Sauk Rivers collected
#' during the Skagit winter steelhead 2021 fishery. The dataset includes angler
#' interviews and reported catch, effort counts, and metadata describing the
#' study design and spatiotemporal fishery configuration. It was used during the
#' development and testing of the statistical models and scripted analysis
#' hosted on <http://github.com/wdfw-fp/CreelEstimates>.
#'
#' @format A named list with 6 elements:
#' \describe{
#'   \item{effort}{A tibble with 4,864 rows and 25 columns containing
#'   vehicle/trailer count data collected at survey locations:
#'     \describe{
#'       \item{creel_event_id}{UUID identifying the creel survey event}
#'       \item{event_date}{Date of the survey event}
#'       \item{water_body}{Name of the water body surveyed}
#'       \item{project_name}{Name of the creel project (e.g., District 14)}
#'       \item{fishery_name}{Name of the fishery (e.g., Skagit winter steelhead 2021)}
#'       \item{effort_event_id}{UUID identifying the individual effort count event}
#'       \item{location}{Name of the count location}
#'       \item{location_id}{UUID identifying the count location}
#'       \item{tie_in_indicator}{Logical; whether the count is a tie-in count}
#'       \item{count_sequence}{Sequence number of the count within the event}
#'       \item{effort_start_time}{Start time of the count period (hms)}
#'       \item{effort_end_time}{End time of the count period (hms)}
#'       \item{no_count_reason}{Reason a count was not completed, if applicable}
#'       \item{comments}{Free-text comments on the count}
#'       \item{count_type}{Type of count (e.g., "Vehicle Only", "Trailers Only")}
#'       \item{count_quantity}{Number of vehicles or trailers counted}
#'       \item{location_type}{Type of location (e.g., "Site")}
#'       \item{survey_type}{Type of survey (e.g., "Index")}
#'       \item{location_season_name}{Season name for the location, if applicable}
#'       \item{section_num}{Section number within the fishery}
#'       \item{surveyor_num}{Surveyor identifier number}
#'       \item{p_census_bank}{Proportion of bank census covered}
#'       \item{p_census_boat}{Proportion of boat census covered}
#'       \item{indirect_census_bank}{Logical; whether bank count is an indirect census}
#'       \item{direct_census_bank}{Logical; whether bank count is a direct census}
#'     }
#'   }
#'   \item{ll}{A tibble with 2 rows and 6 columns containing latitude/longitude
#'   centroid coordinates for each water body:
#'     \describe{
#'       \item{water_body_id}{UUID identifying the water body}
#'       \item{water_body_desc}{Name of the water body}
#'       \item{psc_code}{Pacific Salmon Commission water body code}
#'       \item{llid}{Latitude/longitude identifier for the water body}
#'       \item{centroid_lat}{Latitude of the water body centroid (decimal degrees)}
#'       \item{centroid_lon}{Longitude of the water body centroid (decimal degrees)}
#'     }
#'   }
#'   \item{interview}{A tibble with 1,860 rows and 39 columns containing
#'   angler interview data:
#'     \describe{
#'       \item{creel_event_id}{UUID identifying the creel survey event}
#'       \item{event_date}{Date of the interview}
#'       \item{water_body}{Name of the water body}
#'       \item{project_name}{Name of the creel project}
#'       \item{fishery_name}{Name of the fishery}
#'       \item{interview_id}{UUID identifying the individual interview}
#'       \item{interview_number}{Sequential interview number within the event}
#'       \item{angler_type}{Type of angler (e.g., "bank", "boat")}
#'       \item{interview_location}{Description of where the interview took place}
#'       \item{fishing_location}{Location where the angler was fishing}
#'       \item{location_id}{UUID identifying the interview location}
#'       \item{crc_area}{CRC catch record card area code}
#'       \item{angler_count}{Number of anglers in the interviewed party}
#'       \item{total_group_count}{Total count of individuals in the group}
#'       \item{pole_count}{Number of poles/rods in use}
#'       \item{trip_status}{Whether the trip was complete or incomplete at interview time}
#'       \item{fishing_start_time}{Time the angler began fishing (hms)}
#'       \item{fishing_end_time}{Time the angler finished fishing (hms)}
#'       \item{interview_time}{Time the interview was conducted (hms)}
#'       \item{target_species}{Species the angler was targeting}
#'       \item{trip_guided}{Whether the trip was professionally guided or non-guided}
#'       \item{boat_type}{Type of boat used, if applicable}
#'       \item{boat_used}{Whether a boat was used ("Yes"/"No")}
#'       \item{fish_from_boat}{Logical; whether the angler fished from a boat}
#'       \item{vehicle_count}{Number of vehicles associated with the party}
#'       \item{trailer_count}{Number of trailers associated with the party}
#'       \item{state_residence}{State of residence of the angler}
#'       \item{zip_code}{ZIP code of the angler's residence}
#'       \item{previously_interviewed}{Logical; whether the angler was interviewed earlier in the day}
#'       \item{comment_txt}{Free-text comments on the interview}
#'       \item{location_type}{Type of location (e.g., "Site", "Section)}
#'       \item{survey_type}{Type of survey (e.g., "Census", "Index")}
#'       \item{location_season_name}{Season name for the location, if applicable}
#'       \item{section_num}{Section number within the fishery}
#'       \item{surveyor_num}{Surveyor number}
#'       \item{p_census_bank}{Proportion of bank census covered}
#'       \item{p_census_boat}{Proportion of boat census covered}
#'       \item{indirect_census_bank}{Logical; whether bank count is an indirect census}
#'       \item{direct_census_bank}{Logical; whether bank count is a direct census}
#'     }
#'   }
#'   \item{catch}{A tibble with 683 rows and 11 columns containing catch
#'   and release data from angler interviews:
#'     \describe{
#'       \item{interview_id}{UUID linking the catch record to an interview}
#'       \item{catch_id}{UUID identifying the individual catch record}
#'       \item{species}{Species caught (e.g., "Steelhead", "Bull Trout")}
#'       \item{run}{Stock run designation, if recorded}
#'       \item{life_stage}{Life stage of the fish, if recorded}
#'       \item{fin_mark}{Fin clip mark (e.g., "UM" for unmarked)}
#'       \item{sex}{Sex of the fish, if recorded}
#'       \item{fork_length_cm}{Fork length of the fish in centimeters, if recorded}
#'       \item{fate}{Disposition of the fish (e.g., "Released")}
#'       \item{fish_count}{Number of fish in the catch group}
#'       \item{catch_group}{Concatenated key describing the catch group (species, run, fin mark, fate)}
#'     }
#'   }
#'   \item{closures}{A tibble with 60 rows and 3 columns containing
#'   dates when fishery sections were closed:
#'     \describe{
#'       \item{fishery_name}{Name of the fishery}
#'       \item{section_num}{Section number that was closed}
#'       \item{event_date}{Date of the closure (POSIXct)}
#'     }
#'   }
#'   \item{fishery_manager}{A tibble with 22 rows and 11 columns containing
#'   fishery management configuration, including survey locations and census parameters:
#'     \describe{
#'       \item{project_name}{Name of the creel project}
#'       \item{fishery_name}{Name of the fishery}
#'       \item{location_code}{Description of the survey location}
#'       \item{location_type}{Type of location (e.g., "Site", "Section)}
#'       \item{survey_type}{Type of survey (e.g., "Census", "Index")}
#'       \item{section_num}{Section number within the fishery}
#'       \item{surveyor_num}{Surveyor number}
#'       \item{p_census_bank}{Proportion of bank census assigned to this location}
#'       \item{p_census_boat}{Proportion of boat census assigned to this location}
#'       \item{catch_area_description}{Name of the catch record card reporting area}
#'       \item{catch_area_code}{Numeric catch record card area code}
#'     }
#'   }
#' }
#' @source State of Washington's Open Data Portal, <https://data.wa.gov>
"example_data"
