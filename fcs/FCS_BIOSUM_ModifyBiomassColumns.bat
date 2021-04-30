@echo on
SET DRIVE=%~d0
SET CURRENTFOLDER="%~dp0%"
sqlplus system/admin@xe @FCS_BIOSUM_ModifyBiomassColumns.SQL
EXIT
