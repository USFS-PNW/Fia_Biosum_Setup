@echo on
rem Drop the FCS_BIOSUM User.
sqlplus system/admin@xe @FCS_BIOSUM_TryDropUserCmd.SQL
rem Copy the binary dump file to the default DATA_PUMP_DIR for XE.
copy FCS_BIOSUM.dmp C:\oraclexe\app\oracle\admin\XE\dpdump\*.*
rem Execute the Data Pump Import of the FCS dump file.
C:\oraclexe\app\oracle\product\11.2.0\server\bin\impdp.exe system/admin@xe parfile=FCS_BIOSUM_impdp.dat
sqlplus system/admin@xe @FCS_BIOSUM_SetNoPasswordExpiration.SQL
