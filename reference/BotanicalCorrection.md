# Botanical Correction

Botanical Correction

## Usage

``` r
BotanicalCorrection(Data, Sources = c("wfo"))
```

## Arguments

- Data:

  Dataset (data.frame or data.table); it must contain Site and IdTree

- Sources:

  Character vector. Taxonomic source to use. Only option is "wfo"

## Value

A list with a complete log of the botanical corrections and Data with
new columns:

- `Accepted_family_DataHarmonizationCor` (character): corrected family
  name

- `Accepted_genus_DataHarmonizationCor` (character): corrected genus
  name

- `Accepted_species_DataHarmonizationCor` (character): corrected species
  name

- `Accepted_name_DataHarmonizationCor` (character): corrected name (at
  any taxonomic level)

- `Accepted_name_rank_DataHarmonizationCor` (character): Taxonomic rank
  of name matched by the TNRS

## Details

- The function uses TNRS package provided by BIEN,
  https://bien.nceas.ucsb.edu/bien/tools/tnrs/

- It resolves plant taxonomic names using World Flora Online

- No special characters (typography)

- The suffix "aceae" is restricted to families, and words ending with
  "aceae" are deleted anywhere else

- Correct spelling of botanical names

- Update the families according to APG

- The log contains a flag if there is more than one name for one Site x
  IdTree combination

- The log contains all details returned by TNRS (authors, etc.). Data
  just keeps the basic corrected names.

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
data(TestData)
Rslt <- BotanicalCorrection(TestData)
} # }

```
