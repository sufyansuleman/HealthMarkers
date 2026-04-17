# Detect a participant/sample ID column in a data frame

Checks column names against common patterns (case-insensitive exact
match): id, iid, participant_id, subject_id, sample_id, pid, sid,
record_id. Returns the first matching column name, or NULL if none
found.

## Usage

``` r
.hm_detect_id_col(data)
```
