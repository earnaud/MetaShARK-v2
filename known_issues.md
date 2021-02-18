# Known issues

This document describes the encountered issues and the way they have been adressed (and hopefully fixed). Any issue is described with: a title (quoted from the error message), a timestamp of when the issue record was lastly edted, the typical error message, possibly some comments and a way it has been adressed (possibly with multiple solutions for different locations in the code).

## "Input has no columns; doing nothing"

Timestamp: 2020/12/08 13:21 (UTC+1, Paris)

### Message 

```
Warning in data.table::fwrite(.keywords, paste0(.sv$SelectDP$dp.metadata.path,  :
  Input has no columns; doing nothing.
If you intended to overwrite the file at /home/pndb-elie/dataPackagesOutput/emlAssemblyLine/2020-12-08_elie_kakila_emldp/2020-12-08_elie_kakila/metadata_templates/keywords.txt with an empty one, please use file.remove first.
```

### Comment

Warning message. Happens in Misc (8) if keywords are not filled while saving.

### Fix

Nothing.

-- template --

## ""

Timestamp

### Message

### Comment

### Fix
