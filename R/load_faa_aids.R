#' Load a FAR parts file
#'
#' This function reads a TSV format text file containing a table of numbered
#' parts of the U.S. Federal Aviation Regulations (FAR) and their
#' corresponding descriptions. It expects FAR part numbers in the first column
#' and descriptions in the second column, without a header row of column names.
#' If no file path is specified, it will read the file provided by the package.
#' It returns a named vector suitable for passing to forcats::fct_recode() as
#' the ... <dynamic-dots> argument.
#'
#' @param farparts_file Path to the TSV file of FAR parts
#' @return A named vector usable as ... of forcats::fct_recode()
load_farparts <- function(farparts_file) {
  readr::read_tsv(
    farparts_file, show_col_types = FALSE, col_names = FALSE
  ) |>
    dplyr::mutate( # tidyr::pivot_wider needs unique names
      X2 = make.unique(X2)
    ) |>
    tidyr::pivot_wider( # Pivot to create named vector
      values_from = X1, names_from = X2
    )
}

#' Load AID code table files
#'
#' This function reads all TSV format text files in a specified directory that
#' match a specified regular expression pattern. It expects the regex pattern
#' to have a capture group that is unique to each file. It expects each file to
#' contain a column of codes named CODE and a column of descriptions named
#' LONG DESCRIPTION. It returns a named vector (named for the regex capture
#' group contents) of named vectors, each of which is suitable for passing to
#' forcats::fct_recode() as the ... <dynamic-dots> argument.
#'
#' @param code_dir Directory containing AID code table files
#' @param code_file_pattern Regex pattern for AID code table filenames
#' @return A named vector of vectors for forcats::fct_recode()
load_aidcodes <- function(code_dir, code_file_pattern) {
  code_files <- list.files( # Enlist AID code table files
    code_dir,
    pattern = code_file_pattern,
    ignore.case = FALSE,
    full.names = TRUE
  )

  aidcodes <- code_files |>
    purrr::map( # Read TSV data from each AID code table file
      \(f) {
        readr::read_tsv(
          f,
          show_col_types = FALSE,
          col_types = readr::cols_only(CODE = "c", `LONG DESCRIPTION` = "c")
        ) |>
          dplyr::mutate( # tidyr::pivot_wider needs unique names
            `LONG DESCRIPTION` = make.unique(`LONG DESCRIPTION`)
          ) |>
          tidyr::pivot_wider( # Pivot to create vector for forcats::fct_recode()
            values_from = CODE, names_from = `LONG DESCRIPTION`
          )
      }
    )

  # Capture table names from file names using regex capture group
  names(aidcodes) <- stringr::str_match(code_files, code_file_pattern)[, 2]

  aidcodes
}

#' Load AIDS data files
#'
#' This function reads TSV format text files published by the U.S. Federal
#' Aviation Administration (FAA) containing data from their Accident/Incident
#' Data System (AIDS), and it decodes factor values using the FAA's published
#' code tables. See also documentation for load_aidcodes() and load_farparts().
#'
#' @param data_dir Directory with FAA data files (defaults to package data)
#' @param a_file_pattern Regex pattern for FAA AIDS A (main data) filenames
#' @param e_file_pattern Regex pattern for FAA AIDS E (remarks) filenames
#' @return A tidyr::tibble of FAA AIDS data
#' @export
load_aids <- function(
  data_dir = system.file("extdata", package = "faa.aids"),
  a_file_pattern = "^a.*[0-9]{2}\\.txt",
  e_file_pattern = "^e.*[0-9]{2}\\.txt"
) {
  farparts <- load_farparts( # Load descriptions of FAR parts
    system.file("extdata", "farparts.txt", package = "faa.aids")
  )

  aidcodes <- load_aidcodes( # Load files of FAA AID code tables
    system.file("extdata", package = "faa.aids"), "aidcodes-([A-Z]*)\\.txt"
  )

  a_files <- data_dir |>
    list.files( # Enlist A files (main data)
      pattern = a_file_pattern, ignore.case = TRUE, full.names = TRUE
    )

  e_files <- data_dir |>
    list.files( # Enlist E files (remarks)
      pattern = e_file_pattern, ignore.case = TRUE, full.names = TRUE
    )

  a <- a_files |>
    purrr::map( # Read TSV data from each A file
      \(f) {
        readr::read_tsv(
          f,
          show_col_types = FALSE,
          col_types = readr::cols_only(
            c1   = "c",
            c2   = "c",
            c5   = "c",
            c9   = readr::col_date(format = "%Y%m%d"),
            c10  = readr::col_time(format = "%H%M"),
            c11  = "c",
            c12  = "c",
            c13  = "c",
            c14  = "c",
            c20  = "c",
            c21  = "c",
            c22  = "c",
            c23  = "c",
            c24  = "c",
            c76  = "c",
            c77  = "c",
            c78  = "c",
            c94  = "c",
            c95  = "c",
            c96  = "c",
            c97  = "c",
            c98  = "c",
            c99  = "c",
            c100 = "c",
            c101 = "c",
            c102 = "c",
            c103 = "c",
            c104 = "c",
            c105 = "c",
            c106 = "c",
            c107 = "c",
            c108 = "c",
            c109 = "c",
            c110 = "c",
            c145 = "c",
            c146 = "c",
            c147 = "c",
            c148 = "c",
            c149 = "c",
            c150 = "c",
            c151 = "c",
            c153 = "c",
            c154 = "c",
            c155 = "c",
            c156 = "c",
            c157 = "c",
            c158 = "c",
            c160 = "c",
            c161 = "c",
            c162 = "c",
            c163 = "c",
            c183 = "c",
            c184 = "c",
            c191 = "c",
            c192 = "c",
            c233 = "c",
            c244 = "c",
            c250 = "c"
          )
        )
      }
    ) |>
    dplyr::bind_rows()

  e <- e_files |>
    purrr::map( # Read TSV data from each E file
      \(f) readr::read_tsv(f, show_col_types = FALSE)
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate( # Merge post-2020 names with pre-2020 names
      id = dplyr::if_else(is.na(c5), id, c5),
      remark = dplyr::if_else(is.na(remark), rmks, remark),
      .keep = "none"
    )

  a |> # Make meaningful names, and use AID code tables for missing descriptions
    dplyr::mutate(
      `event type` = forcats::fct_recode(
        stringr::str_to_upper(c1), Accident = "A", Incident = "I"
      ),
      `FAR part` = forcats::fct_recode(
        stringr::str_remove(c2, "^0+"), !!!farparts
      ),
      id = c5,
      `event date` = c9,
      `local time` = c10,
      region = forcats::fct_recode(c11, !!!aidcodes$REG),
      `district office` = forcats::fct_recode(c12, !!!aidcodes$DO),
      state = forcats::fct_recode(c13, !!!aidcodes$ST),
      city = c14,
      latitude = c20,
      longitude = c21,
      `aircraft registration` = c22,
      `aircraft make` = c23,
      `aircraft model` = c24,
      `total deaths` = as.double(c76),
      `primary cause factor` = dplyr::if_else(
        is.na(c77), forcats::fct_recode(c78, !!!aidcodes$CF), c77
      ),
      `accident type` = forcats::fct_recode(c94, !!!aidcodes$TYP),
      `flight phase` = dplyr::if_else(
        is.na(c95), forcats::fct_recode(c96, !!!aidcodes$PHS), c95
      ),
      damage = dplyr::if_else(
        is.na(c97), forcats::fct_recode(c98, !!!aidcodes$DAM), c97
      ),
      `cause category` = dplyr::if_else(
        is.na(c99), forcats::fct_recode(c100, !!!aidcodes$GCC), c99
      ),
      `flying type` = dplyr::if_else(
        is.na(c101),
        stringr::str_to_title(forcats::fct_recode(c102, !!!aidcodes$TFP)),
        c101
      ),
      `secondary flying type` = dplyr::if_else(
        is.na(c103),
        stringr::str_to_title(forcats::fct_recode(c104, !!!aidcodes$TFS)),
        c103
      ),
      `flying condition` = dplyr::if_else(
        is.na(c105), forcats::fct_recode(c106, !!!aidcodes$FCP), c105
      ),
      `secondary flying condition` = dplyr::if_else(
        is.na(c107), forcats::fct_recode(c108, !!!aidcodes$FCS), c107
      ),
      `light condition` = dplyr::if_else(
        is.na(c109), forcats::fct_recode(c110, !!!aidcodes$LC), c109
      ),
      `weight class` = dplyr::if_else(
        is.na(c146), forcats::fct_recode(c145, !!!aidcodes$WGT), c146
      ),
      `wing type` = dplyr::if_else(
        is.na(c148), forcats::fct_recode(c147, !!!aidcodes$WING), c148
      ),
      `power class` = dplyr::if_else(
        is.na(c150), forcats::fct_recode(c149, !!!aidcodes$PWRCL), c150
      ),
      `engine count` = as.double(c151),
      `engine power` = dplyr::if_else(
        is.na(c154), forcats::fct_recode(c153, !!!aidcodes$EPWCL), c154
      ),
      `engine type` = dplyr::if_else(
        is.na(c156), forcats::fct_recode(c155, !!!aidcodes$ETYP), c156
      ),
      `landing gear type` = dplyr::if_else(
        is.na(c158), forcats::fct_recode(c157, !!!aidcodes$LGCD), c158
      ),
      `additional cause factor` = dplyr::if_else(
        is.na(c161), forcats::fct_recode(c160, !!!aidcodes$OTH), c161
      ),
      `second additional cause factor` = dplyr::if_else(
        is.na(c163), forcats::fct_recode(c162, !!!aidcodes$OTH), c163
      ),
      `supporting factor A` = dplyr::if_else(
        is.na(c183), forcats::fct_recode(c184, !!!aidcodes$SUP), c183
      ),
      `supporting factor B` = dplyr::if_else(
        is.na(c191), forcats::fct_recode(c192, !!!aidcodes$SUP), c191
      ),
      `pilot died` = stringr::str_to_upper(c233),
      `flight plan` = forcats::fct_recode(c244, !!!aidcodes$PLN),
      `total injuries` = as.double(c250),
      .keep = "none"
    ) |>
    dplyr::mutate( # Clean up 4-digit FCP codes not described in code table
      `primary cause factor` = dplyr::if_else(
        stringr::str_length(`primary cause factor`) < 5,
        NA,
        `primary cause factor`
      )
    ) |>
    purrr::modify_if( # Clean up NA equivalents
      is.character,
      ~ dplyr::if_else(
        stringr::str_detect(., "(?i)^unknown|^undetermined"), NA, .
      )
    ) |>
    dplyr::left_join(e, by = dplyr::join_by(id)) # Join with remarks
}
