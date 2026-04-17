#!/bin/bash

while IFS= read -r file; do
    codex exec "In the file $file, replace all instances of '.sas7bdat' with '.csv' and all instances of 'read_sas' with 'read_csv'." --skip-git-repo-check
done < files_to_update.txt
