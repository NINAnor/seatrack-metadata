# Example script

## Setup

Loading the library:

``` r

library(seatrackRtools)
```

There are a few things that need to be set before the functions can be
used.

It is important that we can monitor changes made and the reasoning
behind them. This package uses the
[logger](https://cran.r-project.org/web/packages/logger/index.html)
package for logging. This will print messages to the console and write
them to a file.

``` r

start_logging()
```

By default the logging level is set to **INFO**. You can also set the
scripts to be more verbose by changing the log threshold using commands
from the logger library:

``` r

library(logger)
log_threshold(TRACE)
```

For more information about different logger levels see
[log_levels](https://daroczig.github.io/logger/reference/log_levels.html).

You also need to point the package to where the seatrack directory is
placed in your file system. This variable will be used by many functions
contained in this package.

``` r

path_to_seatrack <- file.path("a_filepath", "SEATRACK - shared")
set_sea_track_folder(path_to_seatrack)
```

    #> INFO [2026-07-07 14:13:04] Sea track folder set to: /tmp/RtmpcBvC15/seatrack_vignette_2cb165d82915/SEATRACK - shared

## Loading data

Once this is set up you can open a partner metadata file. Note that the
files shown in this vignette are missing a number of columns present in
the real files.

You can check for non_processed sheets by location, or just enter a path
directly.

``` r

unprocessed_metadata_paths <- get_location_unprocessed("location_name")
```

The partner metadata excel can then be loaded into R.

``` r

partner_data <- load_partner_metadata(partner_xlsx)
#> Warning in (function (file, sheet, start_row = NULL, start_col = NULL,
#> row_names = FALSE, : variable from `types` not found in data
#> Warning in (function (file, sheet, start_row = NULL, start_col = NULL,
#> row_names = FALSE, : variable from `types` not found in data
```

You can then load the master file that you wish to update. This can be
done by using the colony name.

``` r

master_import <- load_master_import("TestColony")
#> INFO [2026-07-07 14:13:05] Get Master import file for colony 'TestColony', use existing paths: TRUE
#> INFO [2026-07-07 14:13:05] Master import file for colony 'TestColony' found at: /tmp/RtmpcBvC15/seatrack_vignette_2cb165d82915/SEATRACK - shared/Database/Imports_Metadata/imports_TestColony_2025.xlsx
```

The function returns a class with two elements, `data` and `wb`. `wb` is
the original excel workbook from which the data was loaded. `data` is a
list, where each element is a sheet from the imported master file.

``` r

names(master_import$data)
#> [1] "METADATA"         "STARTUP_SHUTDOWN"
head(master_import$data$METADATA)
#> # A tibble: 1 × 8
#>   date       ring_number logger_model_deployed logger_id_deployed
#>   <date>     <chr>       <chr>                 <chr>             
#> 1 2024-01-10 42          birdTracker5000       L1                
#> # ℹ 4 more variables: logger_model_retrieved <dbl>, logger_id_retrieved <dbl>,
#> #   colony <chr>, comment <chr>
```

## Workflow

There are several steps to updating the master import sheet. Firstly
checking startup files for missing logger sessions.

``` r

updated_startup <- add_loggers_from_startup(master_import, partner_data)
#> ## seatrackR is up to date.
#> WARN [2026-07-07 14:13:05] No database connection from which to get models
#> INFO [2026-07-07 14:13:05] Logger ID L1 was retrieved on 2025-01-10 but no startup was added. 
#> This falls into a single open session started on 2024-01-01 . The retrieval may belong to this open session
#> SUCCESS [2026-07-07 14:13:05] Adding 0 new loggers from startup files
```

Then appending reported encounter data.

``` r

updated_metadata <- append_encounter_data(
  master_import$data$METADATA,
  partner_data$data$`ENCOUNTER DATA`
)
#> SUCCESS [2026-07-07 14:13:05] Appended 1 rows to master metadata. New total is 2 rows.
```

Finally the processing reported logger returns and attempts to update
the appropriate session in the master startup sheet, setting download
status and dates.

This function also handles loggers that the partner restearts,
generating new sessions for them in the master startup sheet.

The function will also update the nonresponsive lists.

``` r

updated_sessions <- handle_returned_loggers(
  "TestColony",
  updated_startup,
  partner_data$data$`LOGGER RETURNS`,
  partner_data$data$`RESTART TIMES`,
)
#> SUCCESS [2026-07-07 14:13:05] Found unfinished session for logger ID: L1 2025-01-10
#> SUCCESS [2026-07-07 14:13:05] Unfinished session:
#>   logger_serial_no starttime_gmt       intended_species intended_location
#> 1 L1               2024-01-01 00:00:00 bird             TestColony       
#> SUCCESS [2026-07-07 14:13:05] Updated 1 sessions.
#> SUCCESS [2026-07-07 14:13:05] Updated sessions:
#>   logger_serial_no starttime_gmt       download_type download_date
#> 1 L1               2024-01-01 00:00:00 Downloaded    2025-01-10
```

At every stage, the log will report the changes that the functions have
made to the master sheet. The log will also report when it is unable to
handle some part of the partner supplied metadata. This can help
identify areas that need manually fixing in either the partner metadata
sheet or the master sheet.

## Combined workflow

This workflow can be run using a single function,
`handle_partner_metadata` that takes the colony name and the imported
excel files as arguments. It returns a list containing the updated
master sheets, and the updated nonresponsive sheets.

``` r

new_sheets <- handle_partner_metadata(
  "TestColony",
  partner_data,
  master_import
)
#> INFO [2026-07-07 14:13:05] Handle partner metadata /tmp/RtmpcBvC15/seatrack_vignette_2cb165d82915/Metadata_SEATRACK_2025-TestColony.xlsx 
#> for TestColony /tmp/RtmpcBvC15/seatrack_vignette_2cb165d82915/SEATRACK - shared/Database/Imports_Metadata/imports_TestColony_2025.xlsx
#> INFO [2026-07-07 14:13:05] Add missing sessions from start up files
#> WARN [2026-07-07 14:13:05] No database connection from which to get models
#> INFO [2026-07-07 14:13:05] Logger ID L1 was retrieved on 2025-01-10 but no startup was added. 
#> This falls into a single open session started on 2024-01-01 . The retrieval may belong to this open session
#> SUCCESS [2026-07-07 14:13:05] Adding 0 new loggers from startup files
#> INFO [2026-07-07 14:13:05] Check for duplicate sessions
#> INFO [2026-07-07 14:13:05] Append encounter data
#> SUCCESS [2026-07-07 14:13:05] Appended 1 rows to master metadata. New total is 2 rows.
#> INFO [2026-07-07 14:13:05] Update sessions from logger returns
#> SUCCESS [2026-07-07 14:13:05] Found unfinished session for logger ID: L1 2025-01-10
#> SUCCESS [2026-07-07 14:13:05] Unfinished session:
#>   logger_serial_no starttime_gmt       intended_species intended_location
#> 1 L1               2024-01-01 00:00:00 bird             TestColony       
#> SUCCESS [2026-07-07 14:13:05] Updated 1 sessions.
#> SUCCESS [2026-07-07 14:13:05] Updated sessions:
#>   logger_serial_no starttime_gmt       download_type download_date
#> 1 L1               2024-01-01 00:00:00 Downloaded    2025-01-10   
#> INFO [2026-07-07 14:13:05] Finished handling partner metadata /tmp/RtmpcBvC15/seatrack_vignette_2cb165d82915/Metadata_SEATRACK_2025-TestColony.xlsx 
#> for TestColony /tmp/RtmpcBvC15/seatrack_vignette_2cb165d82915/SEATRACK - shared/Database/Imports_Metadata/imports_TestColony_2025.xlsx
new_master_sheets <- new_sheets$master_import
```

Finally, if you are happy with the changes made, you can save the
updated master sheet.

``` r

save_master_sheet(new_master_sheets)
```
