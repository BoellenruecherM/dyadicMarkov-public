# Sensitivity-analysis simulation datasets

Fixed archived simulation datasets used in the sensitivity-analysis
vignette for sequence lengths 30, 60, 90, 180, and 720. The datasets
contain binary bivariate dyadic sequences and are re-analysed by the
current package; they are not re-simulated during package use or
testing.

## Format

Each object is a data frame with 4,000 rows, representing 1,000 dyads
observed for two members and two variables. The columns `TM1`, ...,
`TMn` contain the simulated binary states for the corresponding sequence
length. The remaining columns are `members`, `variable`,
`caseSimulated`, `dyad`, and `local`. The five datasets were simulated
separately for sequence lengths 30, 60, 90, 180, and 720 and are
therefore not nested prefixes of one another. The `caseSimulated` column
records the interaction pattern used to simulate each member-variable
sequence, whereas `local` records the archived classification produced
by the historical analysis. Historical terminology is retained in these
fixed simulation datasets; current package output uses the current
scientific nomenclature.
