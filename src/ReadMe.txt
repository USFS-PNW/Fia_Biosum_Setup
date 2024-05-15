### FULL INSTALLATION PACKAGE ###
This project contains the configuration to build an installer (.msi) file
for the FIA Biosum Manager. The following required components should be installed
prior to installing the .msi:

1. R (plus the RSQLite package)
2. The Access Database Engine
3. A Java SDK (v1.8 or later)  
4. The required FICS components are in the FCS folder (BioSumComps.jar, BiosumSpeciesConfig.db, fcs_tree.db, fcs_tree_calc.bat)
5. An SQLite ODBC driver

Please consult the Biosum_Setup_Instructions in the docs directory for 
further details.

### UPDATING THE VERSION NUMBER ###
When the version number is changed, it needs to be changed in the following files:

1. USFS-PNW/Fia_Biosum_Setup Deployment Project Properties. Don't forget to update Product Code for new version
2. USFS-PNW/Fia_Biosum_Setup File system editor\User's Desktop shortcut name
3. USFS-PNW/Fia_Biosum_Setup File system editor\User's Programs Menu\FIA Biosum shortcut name
4. USFS-PNW/Fia_Biosum_Setup Property pages\Output file name
5. Update Help/RELEASE_NOTES.xps with current information about release
6. USFS-PNW/Fia-Biosum-Manager src\frmMain g_strAppVer variable (Note: this also updates version in frmAbout)
7. USFS-PNW/Fia-Biosum-Manager src\version_control.cs PerformVersionCheck function to show 
   which database versions are compatible with the new application version
8. biosum_ref.accdb is maintained in Box because it exceeds the GitHub maximum file size: BioSumBox / Development / BioSumRef
9. Always place a current copy of this file in USFS-PNW/Fia_Biosum_Setup/db so that the build can find it
10. If biosum_ref.accdb is updated, REF_VERSION.version_num needs to be incremented
11. USFS-PNW/Fia-Biosum-Manager src\frmMain g_intRefDbVer variable needs to match REF_VERSION.version_num
12: Sign the .msi if it is for wide distribution