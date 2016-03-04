@echo on
rem Drop the FCS User.
sqlplus system/admin@xe @TryDropUserCmd.SQL
rem Copy the binary dump file to the default DATA_PUMP_DIR for XE.
copy FCS_expdp.dmp C:\oraclexe\app\oracle\admin\XE\dpdump\*.*
rem Execute the Data Pump Import of the FCS dump file.
C:\oraclexe\app\oracle\product\11.2.0\server\bin\impdp.exe system/admin@xe parfile=FCS_impdp.DAT
sqlplus system/admin@xe @SetNoPasswordExpiration.SQL
