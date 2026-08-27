# NCRNWater

This is an R package used to process water quality monitoring data.

## Inputs

1) WQX=T
    - One CSV of water quality monitoring data in Water Quality Portal Export-format
    - One CSV of metadata
    - The column names in your files must be identical to those in the example data

## Outputs

## NEWS

### NCRNWater 1.0.2 2026-08-25

#### New features
Example data are provided:
- **NCRNWater::use_example_data(assign = TRUE)** Reads example data into dataframes: `wqp` and `wqp_metadata`.
- **NCRNWater::use_example_data(assign = FALSE)** Reads example into a list of dataframes: `example$wqp` and `example$wqp_metadata`.
Build a ready-to-use NCRNWater object from example data.
*Option 1*
- **mydata <- NCRNWater::example_ncrnwater()**
*Option 2*
-**paths <- NCRNWater::example_paths()**
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
