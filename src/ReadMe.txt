### FULL INSTALLATION PACKAGE ###
This project contains the configuration to build an installer (.msi) file
for the FIA Biosum Manager. In practice, this installer is distributed in
a self-extracting WinZip file that also includes the following required
components:

1. R
2. The Access Database Engine
3. The required FICS components are in the FCS folder (BioSumComps.jar, BiosumSpeciesConfig.db, fcs_tree.db, fcs_tree_calc.bat)

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
8. If USFS-PNW/Fia_Biosum_Setup\db\biosum_ref.accdb is updated, REF_VERSION.version_num needs to be incremented
9. USFS-PNW/Fia-Biosum-Manager src\frmMain g_intRefDbVer variable needs to match REF_VERSION.version_num