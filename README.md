# NCRNWater

This is an R package used to process water quality monitoring data.

## Inputs

1) WQX=T
    - One CSV of water quality monitoring data in Water Quality Portal Export-format
    - One CSV of metadata
    - The column names in your files must be identical to those in the example data

## Outputs

## NEWS

### NCRNWater 1.1.1 (2026-11-06)

#### Major Features

##### Shiny application integrated into NCRNWater

The NCRN Water Quality Shiny application is now distributed as part of the NCRNWater package and can be launched directly from R.

```r
NCRNWater::run_shiny_app()
```

Example data and configuration are included for testing and demonstration:

```r
NCRNWater::run_shiny_example()
```

##### Configuration-driven Shiny app

The Shiny application now supports a YAML-based configuration system.

Features include:

- Profile-based configuration (`profile = "NCRN"`)
- User-supplied configuration files
- Runtime configuration overrides
- Automatic application hydration from YAML-defined data sources
- Automatic year-range detection (`min_year: auto`, `max_year: auto`)
- Optional profile-specific content

##### Profile abstraction

The application was refactored to support water-quality datasets beyond NCRN.

Profile-specific assets and settings can now be provided through YAML configuration, including:

- Application title
- Network code
- Network name
- Dataset URL
- Splash screen content
- About pages
- Basemap configuration

#### New Features

##### Exceedance analysis improvements

- Added metadata-driven threshold comparators.
- `exceed()` now supports:
  - `>`
  - `<`
  - `>=`
  - `<=`
- Added `mode = "rows"` to return the subset of measurements that exceeded a threshold.
- Threshold logic now honors metadata-specified comparison operators.
- Exceedance summaries now use comparator-aware language derived from metadata (e.g., "greater than" rather than always "exceeded").

##### New Shiny exceedance service

Added:

```r
build_exceed_site_package()
```

which provides:

- Threshold evaluation
- Exceedance summaries
- Histogram generation
- Accessibility text generation
- Exceedance tables
- Metadata-driven threshold descriptions

for a single park/site/characteristic combination.

##### Optional splash screens

Splash screens are now optional.

Profiles may provide:

```yaml
app:
  splash:
```

but applications launch normally when splash content is absent.

##### Optional About pages

Profiles may provide:

```yaml
app:
  about:
```

to display profile-specific About content.

If no About page is provided, the application automatically hides the About tab.

##### Basemap configuration framework

Added support for YAML-configured map basemaps.

Supported sources include:

- Leaflet provider basemaps
- XYZ raster tile services
- Tokenized URLs using runtime token substitution

Example:

```yaml
map:
  basemaps:
```

Basemap definitions can be supplied per profile.

##### Example-data workflow improvements

Added helper functions:

```r
example_data()
example_paths()
example_ncrnwater()
run_shiny_example()
```

to simplify testing and onboarding.

##### Data validation utilities

Expanded support for pre-flight validation through:

```r
congruency()
diagnoseWaterData()
```

for evaluating the integrity of WQP-formatted datasets and NCRNWater objects.

#### Bug Fixes

##### Duplication fixes

Resolved duplication issues affecting:

- `getParkInfo()`
- `getSiteInfo()`
- `getCharInfo()`
- `getChars()`
- `exceed()`

The fixes ensure stable behavior for both filtered and unfiltered queries while preserving backward compatibility.

##### Threshold alignment fixes

`exceed()` now calculates threshold values on a site-characteristic basis, eliminating threshold recycling and row-alignment issues that could lead to incorrect exceedance counts.

##### Type stability improvements

Improved type consistency throughout metadata accessors.

Examples:

- Numeric fields return numeric values.
- Character fields return character values.
- Data fields return lists.

##### S4 robustness improvements

Internal object traversal now uses:

```r
methods::is()
```

and type-stable iteration patterns to improve reliability and reduce duplicate object retrieval.

#### Shiny Improvements

##### Time-series figure improvements

- Added point-level hover support in Plotly time-series figures.
- Improved handling of multiple observations collected on the same day.
- Added small visual jitter to overlapping points to improve discoverability.
- Switched hover behavior from x-axis aggregation to nearest-point interaction.

##### Exceedance reporting improvements

- Improved comparator-aware narrative text.
- Improved accessibility descriptions.
- Improved handling of lower-threshold and upper-threshold exceedances.

#### Internal Refactoring

##### New service architecture

Added internal services for:

```r
shinyConfig.R
shinyExceedService.R
shinyHydrationService.R
shinyPhotoService.R
shinySplashService.R
shinyTimeService.R
```

These services separate configuration handling, application hydration, exceedance analysis, photo lookups, splash-screen generation, and time-based utilities from UI/server code.

##### Shiny app packaging

Refactored the Shiny application to run from:

```text
inst/shiny/app
```

using package-managed bootstrap state and configuration-driven initialization.

### NCRNWater 1.0.3 2026-10-09

#### New features

Water shiny app included in NCRNWater
#### Running the Shiny app

Use a profile in the package YAML, provide a user YAML, or override keys at runtime.

```r
library(NCRNWater)

# Quick demo (uses the packaged example dataset staged into a temp sandbox)
NCRNWater::run_shiny_example()

# A) Use a packaged profile (expects files.datadir/<network>/wqp.csv etc.)
NCRNWater::run_shiny_app(profile = "NCRN")

# B) Use a user-supplied YAML (full control over paths & settings)
NCRNWater::run_shiny_app(config_file = "/path/to/my/shiny.yml")

# C) Override specific keys at runtime (no YAML required)
NCRNWater::run_shiny_app(
  profile   = "NCRN",
  overrides = list(files = list(datadir = "/path/to/Data"))
)
```

If you want to run the shiny app with your own data, your project needs a folder structure like this
```
Data (required, yaml.files.datadir)
├── NCRN (required, yaml.profile)
│   ├── img (optional, photo viewer, yaml.files.imagesdir)
│   │   ├── ANTI_SHCK_20060228 (1).jpg
│   │   ├── ANTI_SHCK_20060518 (1).jpg
│   │   ├── ANTI_SHCK_20060628 (1).jpg
│   ├── splash (optional, app-loading splash screens, yaml.files.splashdir)
│   │   ├── dwq_NCRN_ANTI_SHCK_2024-06-04_20240604-131442.jpg
│   │   ├── dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg
│   │   └── dwq_NCRN_PRWI_BONE_2024-06-11_20240611-130821.jpg
│   ├── wqp.csv (required, yaml.files.dataname)
└── └── wqp_ncrnwater_metadata.csv (required, yaml.files.metadataname)

```

Example data are provided:
- **NCRNWater::example_data(assign = TRUE)** Reads example data into dataframes: `wqp` and `wqp_metadata`.
- **NCRNWater::example_data(assign = FALSE)** Reads example into a list of dataframes: `example$wqp` and `example$wqp_metadata`.
Build a ready-to-use NCRNWater object from example data.
*Option 1*
- **mydata <- NCRNWater::example_ncrnwater()**
*Option 2*
- **paths <- NCRNWater::example_paths()**
- **mydata <- NCRNWater::importNCRNWater(paths$dir, paths$data, paths$metadata, T)**

- **congruency()** Gives the user feedback about the quality of WQP data before creating a NCRNWater object. Reports inconsistencies between WQP-formatted inputs (e.g., wqp.csv, wqp_ncrnwater_metadata.csv) and known-correct templates. See `?NCRNWater::congruency` for details and usage examples.
- **diagnoseWaterData()** Gives the user feedback about the quality of the NCRNWater object. Reports inconsistencies like disagreements in site attributes (names, lat/lon) and characteristic attributes (names) in a WaterData object. See `?NCRNWater::diagnoseWaterData` for details and usage examples.

#### Fixes & Improvements
- **Feature:** `exceed()` gains configurable operators (>, <, >=, >=) and new mode="rows" which returns the subset of measurements that exceeded the threshold.
- **Feature:** `metadata` offers configurable operators (>, <, >=, >=) to specify tie-breakers for exceedances.
- **Bug Fix:** Resolved duplication of `Site` and `Characteristic` outputs for certain `parkcode/sitecode/charname` combinations (e.g., `NACE / NCRN_NACE_OXRU / pH`) by:
  - Safely flattening lists of S4 objects (`unlist(..., recursive = FALSE)`).
  - Using robust S4 type checks (`methods::is`).
  - **Conditionally deduplicating** by identity at list/Park levels:
    - When any filters are provided (`parkcode`, `sitecode`, `charname`, or `category`), results are deduplicated by identity (`SiteCode` for Sites; `CharName|Category|SampleFraction|Substrate` for Characteristics).
    - When **no filters** are provided (global calls), results **preserve original concatenation** semantics (no deduplication), maintaining backward compatibility (e.g., `getCharInfo(WaterData, info = "LowerPoint")` returns the full-length numeric vector).
- **Type Stability:** `getCharInfo()` now returns appropriate scalar types:
  - Numeric for `LowerPoint`, `UpperPoint`.
  - Character for descriptive fields (e.g., `LowerDescription`, `Units`).
  - Lists for `Data`.
- **API Safety:** `getCharInfo(object = "Site")` method signature now includes `category = NA` to avoid scoping issues.
- **Bug Fix:** Resolved duplication in `getParkInfo()` for both unfiltered and filtered calls.
  - The `object = "list"` method now safely flattens (`unlist(..., recursive = FALSE)`), filters to `Park` S4 objects, and **deduplicates by `ParkCode`**.
  - Unfiltered calls like `getParkInfo(WaterData)` now return **unique** results (e.g., a single "Nat. Cap. Parks - East"), and filtered calls (`parkcode=`) also return deduped scalars.
- **S4 Robustness:** Uses `methods::is` for type checks, and `vapply` for type‑stable outputs.
- Related earlier fixes to duplication in `getSiteInfo()` and `getCharInfo()` retained.
- **`exceed()` alignment & duplication fix:** Thresholds (`LowerPoint`, `UpperPoint`) are now computed **per site–characteristic** group, ensuring exact alignment with `getWData(..., output = "list")`. This eliminates shape mismatches and prevents recycled thresholds producing a single-row output.
- **Row uniqueness:** Post-aggregation, `exceed()` defensively ensures **one row per `Park` + `Site` + `Characteristic` + `Category`**.
- Works with earlier routing/deduplication fixes in `getSiteInfo()`, `getCharInfo()`, `getChars()`, and `getParkInfo()`.
