# =============================================================================
# STATE-SPECIFIC VARIABLE LISTS FOR DATA QUALITY ASSESSMENT
# Generated from State_Heads_20260130_192640.txt
# Purpose: Frequency tables, missingness checks, type conversion tests
# =============================================================================

library(data.table)

# =============================================================================
# ALABAMA
# Sources: UST_UTanks (2).xlsx, UST_Sites (2).xlsx
# =============================================================================
AL_vars <- list(
  tanks = list(
    source = "UST_UTanks (2).xlsx",
    # --- IDs & Keys ---
    id_vars = c(
      "Tank Identification Number",
      "Permit Number",
      "Permittee"
    ),
    # --- Dates (require type conversion) ---
    date_vars = c(
      "a. Estimated Last Use Date",
      "Install Date",
      "Removed Date",
      "Pipe Install Date"
    ),
    # --- Status/Categorical (need frequency tables) ---
    categorical_vars = c(
      "Tank Status",
      "Substance Stored",
      "Petroleum Product",
      "Petroleum Product select",
      "Tank Usage",
      "Tank Construction Material",
      "Tank Construction Material select",
      "Steel Tank Corrosion Protection",
      "Additional CP",
      "Pipe Construction Material",
      "Pipe Manufacturer",
      "Steel Piping Corrosion Protection",
      "Tank Spill Prevention Equipment",
      "Tank Overfill Prevention Equipment",
      "Tank Overfill Prevention Equipment  select",
      "Tank Method of Release Detection Interstitial Moni",
      "Release Detection Type of Interstitial Monitoring",
      "Tank Method of Release Detection",
      "Pressurized Piping Method of of Release Detection",
      "Group I",
      "Group I select",
      "Group II Interstitial Monitoring",
      "Group II Type of Interstitial Monitoring Method",
      "Group II",
      "Group II select",
      "Type of piping used"
    ),
    # --- Summary/Numeric ---
    numeric_vars = c(
      "Summary",
      "Summary - UST - Number of Currently in Use Tanks",
      "Summary - UST - Number of Temporary Closed Tanks",
      "Summary - UST - Number of Permanently Closed Tanks"
    )
  ),
  sites = list(
    source = "UST_Sites (2).xlsx",
    id_vars = c("Number", "Name"),
    address_vars = c(
      "Address", "Address Street", "Address City", "Address State",
      "Address PostalCode", "Address Country", "Addr Line 1", "City",
      "Zip", "State Code"
    ),
    geo_vars = c("Coordinate", "Latitude", "Longitude"),
    categorical_vars = c(
      "County", "District", "Ownership", "Site Types", "Status", "Entity Category"
    )
  )
)

# =============================================================================
# ARKANSAS
# Source: TankStats_web.mdb (Access database, multiple tables)
# =============================================================================
AR_vars <- list(
  # --- Facility/Registration Table ---
  facilities = list(
    source = "TempRFACS_CountryNO",
    id_vars = c("FACILITY_ID", "AFIN", "AFINDash", "OWNER_ID"),
    name_vars = c("OWNER_NAME", "LOC_NAME", "CONTACT_NAME", "CERT_NAME"),
    address_vars = c("LOC_ADDR", "LOC_CITY", "LOC_STATE", "LOC_ZIP"),
    geo_vars = c("LOC_LATITUDE", "LOC_LONGITUDE", "CountyNo", "CountyCAPS"),
    date_vars = c(
      "DATE_RECEIVED", "ENTRY_DATE", "UPDATE_DATE", "DATE_SIGNED", "DATE_REG_CRT_R"
    ),
    flag_vars = c(
      "AMENDED_FLAG", "ABG_FLAG", "BLG_FLAG", "NO_BILL_FLAG",
      "ActiveSiteFlag", "UG_AG_TMP_OUT_FLAG", "UG_TMP_OUT_FLAG",
      "AG_TMP_OUT_FLAG", "UG_PERM_OUT_FLAG", "AG_PERM_OUT_FLAG",
      "AG_IN_USE_FLAG", "UG_IN_USE_FLAG", "FedFlag", "LUSTFLAG"
    ),
    categorical_vars = c(
      "LOC_SIC", "CONTACT_TITLE", "CERT_TITLE", "LEAK_ID",
      "FacilsWithInspectionPix", "FacilsWithInspectionReports"
    ),
    clerk_vars = c("ENTRY_CLERK", "UPDATE_CLERK")
  ),
  
  # --- Underground Tank Table ---
  tanks_ug = list(
    source = "TempTankStats_UG",
    id_vars = c("RSTFacNbr", "TankNbr", "GISLocationsID"),
    numeric_vars = c("Capacity", "NumCompartments"),
    date_vars = c(
      "InstallDate", "TankStatusDate", "TankAssessDate",
      "UT_MOC_RepairedDate", "UT_RD_InstalledDate", "UT_CP_InstalledDate",
      "UT_SOP_InstalledDate", "UP_TYPE_RepairedDate",
      "CCFinalTestDate", "CCInstallDate", "RecCreatedDate", "RecModifiedDate"
    ),
    status_vars = c("TankStatusTypeCode", "TankStatusChangeRsnCode", "TankAssessLeakYN"),
    # --- Material of Construction flags ---
    moc_flags = c(
      "UT_MOC_SteelTF", "UT_MOC_EpoxyTF", "UT_MOC_CompositeTF",
      "UT_MOC_FbrPlasticTF", "UT_MOC_ConcreteTF", "UT_MOC_IntLiningTF",
      "UT_MOC_ExcvLinerTF", "UT_MOC_DblWallTF", "UT_MOC_PolyJacketTF",
      "UT_MOC_UnknownTF", "UT_MOC_OtherDesc"
    ),
    # --- Substance Stored flags ---
    substance_flags = c(
      "UT_SS_EmptyTF", "UT_SS_DieselTF", "UT_SS_KeroseneTF",
      "UT_SS_GasolineTF", "UT_SS_UsedOilTF", "UT_SS_NewOilTF",
      "UT_SS_UnknownTF", "UT_SS_MixtureDesc", "UT_SS_OtherDesc", "UT_SS_CERCLAName"
    ),
    # --- Release Detection flags ---
    release_det_flags = c(
      "UT_RD_MnlTankGageTF", "UT_RD_TightnessTF", "UT_RD_InvControlTF",
      "UT_RD_AutoTankGageTF", "UT_RD_VaporMntrTF", "UT_RD_GWMntrTF",
      "UT_RD_IntstiTF", "UT_RD_UnknownTF", "UT_RD_SIRTF", "UT_RD_OtherDesc"
    ),
    # --- Corrosion Protection flags ---
    cp_flags = c(
      "UT_CP_ExtAsphaltTF", "UT_CP_ExtDieTF", "UT_CP_ExtFRPTF",
      "UT_CP_IntLiningTF", "UT_CP_CPSTF", "UT_CP_ElecIsolTF",
      "UT_CP_UnknownTF", "UT_CP_OtherDesc"
    ),
    # --- Spill/Overfill Prevention flags ---
    sop_flags = c(
      "UT_SOP_BasinTF", "UT_SOP_AutoShutTF", "UT_SOP_AutoFlowTF",
      "UT_SOP_AutoHighTF", "UT_SOP_UnknownTF", "UT_SOP_OtherDesc"
    ),
    # --- Piping Material flags ---
    pipe_moc_flags = c(
      "UP_MOC_BareSteelTF", "UP_MOC_GalvSteelTF", "UP_MOC_FRPTF",
      "UP_MOC_CopperTF", "UP_MOC_DblWallTF", "UP_MOC_SecContainTF",
      "UP_MOC_UnknownTF", "UP_MOC_OtherDesc"
    ),
    # --- Piping Type flags ---
    pipe_type_flags = c(
      "UP_TYPE_PCVTF", "UP_TYPE_TCVTF", "UP_TYPE_PressureTF",
      "UP_TYPE_GravityTF", "UP_TYPE_UnknownTF", "UP_TYPE_OtherDesc"
    ),
    # --- Piping Release Detection flags ---
    pipe_rd_flags = c(
      "UP_RD_VaporMntrTF", "UP_RD_GWMntrTF", "UP_RD_TightnessTF",
      "UP_RD_LineLeakTF", "UP_RD_IntstiTF", "UP_RD_UnknownTF", "UP_RD_OtherDesc"
    ),
    # --- Piping CP flags ---
    pipe_cp_flags = c(
      "UP_CP_CoatedTF", "UP_CP_FRPTF", "UP_CP_CathodicTF",
      "UP_CP_ElectricalTF", "UP_CP_UnknownTF", "UP_CP_OtherDesc"
    ),
    # --- Compliance/Certification ---
    cert_vars = c(
      "CCTestCoLicense", "CCTesterLicense", "CCCntrctCoLicense", "CCCntrctrLicense"
    ),
    # --- Summary flags ---
    summary_flags = c("CP", "SO", "RD", "FedFlag", "UG_HazardousFlag"),
    text_vars = c("TankComment"),
    clerk_vars = c("RecCreatedBy", "RecModifiedBy")
  ),
  
  # --- Aboveground Tank Table ---
  tanks_ag = list(
    source = "TempTankStats_AG",
    id_vars = c("RSTFacNbr", "TankNbr", "GISLocationsID"),
    numeric_vars = c("Capacity", "NumCompartments", "GalsRemaining"),
    date_vars = c(
      "InstallDate", "TankStatusDate", "LastUsedDate",
      "RecCreatedDate", "RecModifiedDate"
    ),
    status_vars = c("TankStatusTypeCode", "RemovedYN"),
    moc_flags = c(
      "AT_MOC_SteelTF", "AT_MOC_ConcreteTF", "AT_MOC_PlasticTF",
      "AT_MOC_UnknownTF", "AT_MOC_OtherDesc"
    ),
    internal_prot_flags = c(
      "AT_IP_CathodicTF", "AT_IP_LiningTF", "AT_IP_NoneTF",
      "AT_IP_UnknownTF", "AT_IP_OtherDesc"
    ),
    external_prot_flags = c(
      "AT_EP_CathodicTF", "AT_EP_PaintedTF", "AT_EP_PlasticTF",
      "AT_EP_NoneTF", "AT_EP_UnknownTF", "AT_EP_OtherDesc"
    ),
    piping_flags = c(
      "AT_PPNG_BareSteelTF", "AT_PPNG_GalvSteelTF", "AT_PPNG_PlasticTF",
      "AT_PPNG_CathodicTF", "AT_PPNG_UnknownTF", "AT_PPNG_OtherDesc"
    ),
    substance_flags = c(
      "AT_SS_EmptyTF", "AT_SS_DieselTF", "AT_SS_KeroseneTF",
      "AT_SS_GasolineTF", "AT_SS_UsedOilTF", "AT_SS_OtherDesc",
      "AT_SS_HazardousTF", "AT_SS_MixtureDesc", "AT_SS_UnknownTF"
    ),
    text_vars = c("TankComment"),
    clerk_vars = c("RecCreatedBy", "RecModifiedBy")
  ),
  
  # --- LUST (Leaking UST) Table ---
  lust = list(
    source = "TempRLUSTLOG1",
    id_vars = c("LUSTNoticeNbr", "LUSTAFIN", "RSTFacNbr", "LUSTOwnerID"),
    name_vars = c("NotifierName", "LocationName", "LUSTOwnerName"),
    address_vars = c(
      "NotifierAddr2", "NotifierAddr3", "NotifierCity", "NotifierState", "NotifierZip",
      "LocationAddr2", "LocationAddr3", "LocationCity", "LocationState", "LocationZip"
    ),
    geo_vars = c(
      "LUSTCounty", "RSTLUSTLatDecimal", "RSTLUSTLongDecimal",
      "RSTLUSTLatDegrees", "RSTLUSTLatMinutes", "RSTLUSTLatSeconds",
      "RSTLUSTLongDegrees", "RSTLUSTLongMinutes", "RSTLUSTLongSeconds",
      "RSTLUSTUTMNorthing", "RSTLUSTUTMEasting", "RSTLUSTUTMZone"
    ),
    date_vars = c(
      "NotifiedDate", "LeakDate", "TechBranchReferDate", "TrustFundDeterminedDate",
      "ReleaseConfirmedDate", "CleanupInitDate", "ISCDate", "SARDate",
      "CAPSubmittedDate", "PublicNoticeIssueDate", "CAPApprovedDate", "NFAIssuedDate",
      "RSTLUSTGISDateMeasured", "dbo_RSTLUSTCorrActn_RecCreatedDate",
      "dbo_RSTLUSTCorrActn_RecModifiedDate"
    ),
    categorical_vars = c(
      "LUSTTankTypeCode", "LUSTDiscoveryMthdCode", "ReleaseStatus",
      "LUSTPriorityScoreCode", "LUSTFundingSourceCode", "LUSTCleanupLeadCode",
      "RSTLUSTSourceCode", "RSTLUSTCauseCode", "LUSTTankTypeDesc"
    ),
    # --- Substance Spilled flags ---
    substance_flags = c(
      "LUST_SS_GasolineTF", "LUST_SS_JetFuelTF", "LUST_SS_KeroseneTF",
      "LUST_SS_DieselTF", "LUST_SS_UsedWasteOilTF", "LUST_SS_PetroleumNOSTF",
      "LUST_SS_UnknownTF", "LUST_SS_CERCLASubst"
    ),
    # --- Damage Detection flags ---
    damage_flags = c(
      "LUST_DD_SfcSoilsTF", "LUST_DD_SfcWaterTF", "LUST_DD_VaporAccumTF",
      "LUST_DD_UtilityImpactTF", "LUST_DD_SubSfcSoilsGWTF", "LUST_DD_DrnkWaterTF",
      "LUST_DD_OtherDesc"
    ),
    # --- Hazard Abatement flags ---
    abatement_flags = c(
      "LUST_HA_EvacTF", "LUST_HA_ProdRmvRcvrTF", "LUST_HA_VaporAbateTF",
      "LUST_HA_ExpsdSoilsTF", "LUST_HA_AltDrnkWaterTF", "LUST_HA_OtherDesc"
    ),
    # --- Remediation Action flags ---
    remediation_flags = c(
      "LUST_RA_ExcvDispTF", "LUST_RA_ExcvTrtTF", "LUST_RA_InSituChemOxTF",
      "LUST_RA_InSituBioremedTF", "LUST_RA_SoilFlushTF", "LUST_RA_NtrlAttenTF",
      "LUST_RA_AirSpargingTF", "LUST_RA_SoilVaporExtTF", "LUST_RA_DualPhaseExtTF",
      "LUST_RA_GWPumpTrtTF", "LUST_RA_OtherDesc"
    ),
    numeric_vars = c("LeakVolume", "AcresReturnedToUse"),
    flag_vars = c("EmergResponseYN", "TrustFundEligYN", "InactiveYN"),
    text_vars = c("LeakDamageDesc", "LUSTComment", "RSTLUSTSourceOtherDesc", "RSTLUSTCauseOtherDesc"),
    phone_vars = c("NotifierPhone", "LocationPhone"),
    clerk_vars = c(
      "dbo_RSTLUSTCorrActn_RecCreatedBy", "dbo_RSTLUSTCorrActn_RecModifiedBy",
      "InactiveBy", "ReceivedByName"
    )
  ),
  
  # --- GIS Table ---
  gis = list(
    source = "TempGIS",
    id_vars = c("GISLocationsID"),
    geo_vars = c(
      "RSTFacLatDecimal", "RSTFacLongDecimal",
      "RSTFacLatDegrees", "RSTFacLatMinutes", "RSTFacLatSeconds",
      "RSTFacLongDegrees", "RSTFacLongMinutes", "RSTFacLongSeconds",
      "RSTFacUTMNorthing", "RSTFacUTMEasting", "RSTFacUTMZone"
    ),
    date_vars = c("RSTFacGISDateMeasured"),
    categorical_vars = c(
      "RSTFacGISOriginalCoordinateSystem", "RSTFacGISOriginalDatumCode",
      "RSTFacGISCurrentDatumCode", "RSTFacGISSourceName",
      "RSTFacGPSReceiverTypeName", "RSTFacSectionTownshipRange"
    ),
    numeric_vars = c(
      "RSTFacGPSReceiverChannels", "RSTFacGISBaseStationDistance",
      "RSTFacGISMinPointPositions", "RSTFacGISPDOPMask",
      "RSTFacGISSNRMask", "RSTFacGISHorizAccuracy"
    ),
    flag_vars = c("RSTFacGISCertifiedMeasurementYN"),
    text_vars = c("RSTFacGISBaseStationName", "RSTFacGISComment", "RSTFacGISCollectorStaffName")
  ),
  
  # --- Owner Table ---
  owners = list(
    source = "TempOwnerList",
    id_vars = c("OwnerID"),
    name_vars = c("OwnerName"),
    address_vars = c(
      "OwnerAddr1", "OwnerAddr2", "OwnerCity", "OwnerState",
      "OwnerZip", "OwnerCountry", "CountyName"
    ),
    date_vars = c(
      "DateNoticeReceived", "InactiveDate", "RecCreatedDate", "RecModifiedDate"
    ),
    categorical_vars = c("OwnerType"),
    flag_vars = c("InactiveYN"),
    phone_vars = c("OwnerPhone"),
    text_vars = c("Comment"),
    clerk_vars = c("InactiveBy", "RecCreatedBy", "RecModifiedBy")
  ),
  
  # --- Eligibility Certificate Table ---
  eligibility = list(
    source = "TempEligCert1",
    id_vars = c("facility_id"),
    date_vars = c("date", "entry_date", "update_date"),
    categorical_vars = c("transaction_code", "EligCertTransDesc"),
    clerk_vars = c("entry_clerk", "update_clerk")
  ),
  
  # --- Inspections Table ---
  inspections = list(
    source = "TempInspectionsList2",
    id_vars = c("FacilID"),
    date_vars = c("InspectionDate"),
    categorical_vars = c("InspectionCode", "InspectionDescription", "InspWebReadyCode"),
    text_vars = c("FileName")
  )
)

# =============================================================================
# COLORADO
# Sources: State CSV + EPA National files
# =============================================================================
CO_vars <- list(
  tanks = list(
    source = "Regulated_Storage_Tanks_in_Colorado__Oil___Public_Safety_20250728.csv",
    id_vars = c("Facility ID", "Tank Name", "Tank Tag"),
    name_vars = c("Facility Name"),
    address_vars = c("Address", "City", "County", "Zip Code"),
    date_vars = c("Installation Date", "Closure Date"),
    categorical_vars = c(
      "Facility Type", "Facility Category", "Tank Type", "Tank Status",
      "Product", "Tank Material", "Tank Wall Type", "Overfill Prevention",
      "Tank Release Detection Method (Primary)", "Tank Release Detection Method (Secondary)",
      "Tank Corrosion Protection Method", "Piping Material", "Piping Wall Type",
      "Piping Type", "Piping System", "Piping Release Detection Method (Primary)",
      "Piping Release Detection Method (Secondary)", "Piping Corrosion Protection",
      "Piping Corrosion Protection at Tank", "Piping Corrosion Protection at Dispenser",
      "Line Leak Detector Type"
    ),
    numeric_vars = c("Capacity (gallons)")
  ),
  releases = list(
    source = "Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv",
    id_vars = c("Release Number", "Legacy Event ID", "Facility ID"),
    name_vars = c("Site Name", "Contact"),
    address_vars = c("Address", "City", "Zip", "State", "County"),
    geo_vars = c("Latitude", "Longitude", "Latitude, Longitude"),
    date_vars = c("Release Date", "Closure_Date"),
    categorical_vars = c(
      "Release Record Type", "Status", "No Further Action Type",
      "Release Source", "Release Cause"
    ),
    contact_vars = c("Phone", "Email"),
    text_vars = c("Release Record Link")
  ),
  epa_usts = list(
    source = "USTs.csv (EPA National)",
    id_vars = c("OBJECTID", "Facility_ID", "Tank_ID"),
    categorical_vars = c("State", "Tank_Status", "Substances", "Tank_Wall_Type"),
    date_vars = c("Installation_Date", "Removal_Date"),
    numeric_vars = c("Capacity")
  ),
  epa_facilities = list(
    source = "Facilities.csv (EPA National)",
    id_vars = c("OBJECTID", "Facility_ID"),
    name_vars = c("Name"),
    address_vars = c("Address", "City", "County", "State", "Zip_Code"),
    geo_vars = c("Latitude", "Longitude", "Coordinate_Source", "Address_Match_Type"),
    categorical_vars = c(
      "LandUse", "Facility_Status", "Within_SPA", "Within_WHPA", "Within_100yr_Floodplain",
      "SPA_Water_Type", "SPA_Facility_Type", "SPA_HUC12",
      "WHPA_Water_Type", "WHPA_Facility_Type", "WHPA_HUC12"
    ),
    numeric_vars = c("Open_USTs", "Closed_USTs", "TOS_USTs", "Population_1500ft", "Private_Wells_1500ft")
  ),
  epa_releases = list(
    source = "Releases.csv (EPA National)",
    id_vars = c("OBJECTID", "Facility_ID", "LUST_ID"),
    name_vars = c("Name"),
    address_vars = c("Address", "City", "County", "Zip_Code", "State"),
    geo_vars = c("Latitude", "Longitude", "Coordinate_Source", "Address_Match_Type"),
    date_vars = c("Reported_Date"),
    categorical_vars = c(
      "Status", "Substance", "LandUse", "Within_SPA", "Within_WHPA", "Within_100yr_Floodplain"
    ),
    numeric_vars = c("Population_within_1500ft", "DomesticWells_within_1500ft"),
    flag_vars = c("Open_", "Closed")
  )
)

# =============================================================================
# LOUISIANA
# Source: LA_record_request.xlsx
# =============================================================================
LA_vars <- list(
  lust = list(
    source = "LA_record_request.xlsx",
    id_vars = c("AI_NUMBER", "INCIDENT_ID", "ACTIVITY_TRACKING_NO"),
    name_vars = c("AI_NAME"),
    address_vars = c("ADDRESS", "CITY", "ZIP_CODE", "PARISH"),
    date_vars = c("CONF_REL_DATE", "NFA_DATE"),
    categorical_vars = c("PROGRAM", "SUBSTANCE", "REFERRED_TO_REM")
  )
)

# =============================================================================
# MAINE
# Source: Multiple TXT files (fixed-width/delimited, needs special parsing)
# NOTE: Maine files appear malformed in head extraction; require custom parsing
# =============================================================================
ME_vars <- list(
  # Based on Maine UST data documentation, typical fields:
  tanks = list(
    source = "Maine TXT files (tank_list.txt, etc.)",
    id_vars = c("facility_id", "tank_id"),
    name_vars = c("facility_name", "owner_name", "operator_name"),
    address_vars = c("address", "city", "zip", "county"),
    geo_vars = c("latitude", "longitude"),
    date_vars = c("install_date", "closure_date", "last_used_date"),
    categorical_vars = c(
      "tank_status", "tank_material", "tank_type", "product_stored",
      "corrosion_protection", "release_detection", "overfill_prevention",
      "piping_material", "piping_type", "regulated_status"
    ),
    numeric_vars = c("capacity")
  ),
  lust = list(
    source = "Maine LUST TXT files",
    id_vars = c("incident_id", "facility_id"),
    date_vars = c("discovery_date", "report_date", "nfa_date"),
    categorical_vars = c("status", "cause", "substance", "how_discovered")
  )
)

# =============================================================================
# MICHIGAN
# Source: UTK Master List 3-3-25.xlsx + EPA Releases.csv
# =============================================================================
MI_vars <- list(
  tanks = list(
    source = "UTK Master List 3-3-25.xlsx",
    # Note: Column names have embedded line breaks (\r\n)
    id_vars = c("Facility\r\n ID", "Old Tank \r\nID Number", "New Tank \r\nID Number"),
    name_vars = c("Facility \r\nName", "Owner \r\nName"),
    fac_address_vars = c(
      "Facility \r\nStreet Number", "Facility \r\nDirection",
      "Facility \r\nStreet Name", "Facility \r\nSuffix Type",
      "Facility \r\nSuffix Direction", "Facility \r\nCity",
      "Facility\r\n State", "Facility\r\n Zip", "Facility \r\nCounty",
      "Facility \r\nRegion"
    ),
    owner_address_vars = c(
      "Owner \r\nAddress", "Owner\r\n City", "Owner\r\n State",
      "Owner\r\n Zip", "Owner\r\nPhone"
    ),
    date_vars = c("Tank \r\nInstal Date", "Tank \r\nRemoval Date"),
    categorical_vars = c(
      "Details \r\nStatus", "Tank \r\nContent",
      "Tank \r\nRelease Detection", "Piping \r\nPiping Release Detection",
      "Piping \r\nPiping Material", "Piping \r\nPiping Type",
      "Tank \r\nConstruction", "Impressed \r\nCurrent"
    ),
    numeric_vars = c("Tank \r\nCompartments", "Tank \r\nCapacity")
  ),
  releases = list(
    source = "Releases.csv (EPA National, filtered for MI)",
    # Same structure as CO epa_releases
    id_vars = c("OBJECTID", "Facility_ID", "LUST_ID"),
    name_vars = c("Name"),
    address_vars = c("Address", "City", "County", "Zip_Code", "State"),
    geo_vars = c("Latitude", "Longitude"),
    date_vars = c("Reported_Date"),
    categorical_vars = c("Status", "Substance"),
    flag_vars = c("Open_", "Closed")
  )
)

# =============================================================================
# NEW JERSEY
# Sources: New Jersey Tanks Data.csv, New Jersey Facility Data.csv, EPA Releases.csv
# =============================================================================
NJ_vars <- list(
  tanks = list(
    source = "New Jersey Tanks Data.csv",
    id_vars = c(
      "OBJECTID", "Preferred ID", "Subject Item ID", "Tank Number",
      "PI ID", "INT DOC ID", "UST Facility ID", "UST TANK ID"
    ),
    date_vars = c("Installed Date", "Out of Service Date", "Dataload Date"),
    categorical_vars = c(
      "Activity", "Tank In Use", "Tank Status Description",
      "Tank Contents", "Parameter CAS", "Tank Structure", "Tank Construction",
      "Tank Monitoring", "Piping Structure", "Piping Construction",
      "Piping Monitoring", "Piping Operation", "Compliance",
      "Well Head Protection Area"
    ),
    flag_vars = c(
      "Overfill Protection Flag", "Spill Containment Flag",
      "Monitoring Compliance Flag", "Upgrade Flag"
    ),
    numeric_vars = c("Tank Volume", "Closure Number", "Substantial Modification Number"),
    text_vars = c("Comments")
  ),
  facilities = list(
    source = "New Jersey Facility Data.csv",
    id_vars = c(
      "OBJECTID", "Preferred ID", "Site ID", "PI ID", "INT DOC ID", "UST Facility ID"
    ),
    name_vars = c("PI Name", "Alias Name"),
    address_vars = c("Address", "Address 2", "Municipality", "County", "Comu Code"),
    # NJ uses State Plane coordinates (EPSG:3424)
    geo_vars = c(
      "NJSPC83 Easting (X)", "NJSPC83 Northing (Y)",
      "Coordinate System Type", "Coordinate System",
      "Location Reference", "Coord Source Organization", "x", "y"
    ),
    date_vars = c("Effective Date", "Expiration Date", "Inspection Date (Last)", "Delivery Ban Issued Date", "Dataload Date"),
    categorical_vars = c(
      "Activity", "Document Status", "Registration Status", "Facility Type",
      "Registration Period", "Inspection Activity", "Delivery Ban",
      "Overall Compliance", "Tank Monitoring Compliance", "Tank Upgrade Compliance",
      "Overfill Protection Compliance", "Spill Containment Compliance",
      "Financial Responsibility", "Financial Responsibility Type",
      "Billing Cycle", "Well Head Protection Area"
    ),
    count_vars = c(
      "Inspections (Count)", "Total USTs (Count)", "Active USTs (Count)",
      "Active Single-Wall UST (Count)", "Active Double-Wall UST (Count)",
      "Active Secondary Containment UST (Count)", "Active Gasoline USTS (Count)",
      "Active Heating Oil USTS (Count)", "Active Waste Oil USTS (Count)",
      "Active Bare Steel (Count)", "Active Cathodic Protection (Count)",
      "Active Fiberglass Steel (Count)", "Active Fiberglass Plastic (Count)",
      "Active Internally Lined (Count)", "Active Other Tank Types (Count)"
    ),
    flag_vars = c(
      "Gasoline USTS (Active)", "Diesel USTS (Active)",
      "Heating Oil USTS (Active)", "Waste Oil USTS (Active)"
    ),
    text_vars = c("Parcels")
  ),
  releases = list(
    source = "Releases.csv (EPA National, filtered for NJ)",
    id_vars = c("OBJECTID", "Facility_ID", "LUST_ID"),
    name_vars = c("Name"),
    address_vars = c("Address", "City", "County", "Zip_Code", "State"),
    geo_vars = c("Latitude", "Longitude"),
    date_vars = c("Reported_Date"),
    categorical_vars = c("Status", "Substance"),
    flag_vars = c("Open_", "Closed")
  )
)

# =============================================================================
# NEW MEXICO
# Sources: EPA National USTs.csv, Facilities.csv + State inspection history
# =============================================================================
NM_vars <- list(
  usts = list(
    source = "USTs.csv (EPA National)",
    id_vars = c("OBJECTID", "Facility_ID", "Tank_ID"),
    categorical_vars = c("State", "Tank_Status", "Substances", "Tank_Wall_Type"),
    date_vars = c("Installation_Date", "Removal_Date"),
    numeric_vars = c("Capacity")
  ),
  facilities = list(
    source = "Facilities.csv (EPA National)",
    id_vars = c("OBJECTID", "Facility_ID"),
    name_vars = c("Name"),
    address_vars = c("Address", "City", "County", "State", "Zip_Code"),
    geo_vars = c("Latitude", "Longitude"),
    categorical_vars = c("Facility_Status", "LandUse", "Within_SPA", "Within_WHPA"),
    numeric_vars = c("Open_USTs", "Closed_USTs", "TOS_USTs")
  ),
  inspections = list(
    source = "2025 01 14 UST Compliance inspections_history 1990 - 2024.xlsx",
    # Note: Column names are generic (...2, ...3, etc.) - needs header row fix
    all_vars = c(
      "Petroleum Storage Tank UST Compliance Inspections Date Range 1990 through 2024",
      "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9"
    )
  )
)

# =============================================================================
# OKLAHOMA
# Sources: OCC_PST_AllTanks.csv, OCC_PST_AllCases.csv
# =============================================================================
OK_vars <- list(
  tanks = list(
    source = "OCC_PST_AllTanks.csv",
    id_vars = c("Facility Number", "Tank Number"),
    name_vars = c("Facility Name", "Owner Name"),
    fac_address_vars = c(
      "Facility Address", "Facility City", "Facility State",
      "Facility ZipCode", "Latitude", "Longitude"
    ),
    owner_address_vars = c(
      "Owner Address", "Owner City", "Owner State", "Owner ZipCode", "Owner Phone"
    ),
    date_vars = c(
      "Tank Installed Date", "Tank Closed Date", "Tank Last Used Date Prior To Closure"
    ),
    categorical_vars = c(
      "Tank Type", "Tank Substance", "Tank Construction", "Tank Material",
      "Tank CP Type", "Tank Dike Type", "Pipe Material", "Pipe Construction",
      "Pipe CP Type", "Pipe Type", "Tank Closure Status", "Tank Status",
      "Tank Inert Material", "Tank Approved For Closure In-Place"
    ),
    flag_vars = c("Pipe Underground", "Pipe Aboveground"),
    numeric_vars = c("Tank Compartments", "Tank Capacity")
  ),
  cases = list(
    source = "OCC_PST_AllCases.csv",
    id_vars = c("Case Number", "Facility Number"),
    name_vars = c("Facility Name"),
    address_vars = c(
      "Facility Address", "Facility City", "Facility County",
      "Facility State", "Facility Zip Code"
    ),
    geo_vars = c("Facility Latitude", "Facility Longitude"),
    date_vars = c("Release Date", "Close Date"),
    categorical_vars = c("Case Status", "Case Type", "Release From Tank Type")
  )
)

# =============================================================================
# TENNESSEE
# Sources: ust_all-tn-compartments.xlsx, ust_all-tn-environmental-sites.xlsx,
#          EPA Facilities.csv, uszips.csv (for county crosswalk)
# =============================================================================
TN_vars <- list(
  compartments = list(
    source = "ust_all-tn-compartments.xlsx",
    id_vars = c("Facility Id Ust", "Tank Id", "Compartment Id", "Gia Owner Id"),
    name_vars = c("Facility Name", "Owner Name"),
    fac_address_vars = c(
      "Facility Address1", "Facility Address2", "Facility City", "Facility Zip"
    ),
    owner_address_vars = c(
      "Address Line 1", "Address Line 2", "City", "State", "Zip"
    ),
    date_vars = c(
      "Date Tank Installed", "Pipe Install Or Rep Date", "Date Last Used",
      "Overfill Installation Date", "Spill Bucket Installation Date",
      "Date Tank Closed", "Date Compartment Closed"
    ),
    categorical_vars = c(
      "Facility Type", "Regulated Status", "Red Tag Reason", "Red Tag Status",
      "Tank Number", "Tank Construction", "Category Of Construction",
      "Compartment Letter", "Status", "Product", "Overfill Prevention",
      "Spill Prevention", "Piping Material", "Piping Type", "Pipe Construction Type",
      "Leak Detection Cat", "Leak Detection Periodic", "How Tank Closed",
      "Compartment Release Detection", "Owner Type"
    ),
    flag_vars = c("Emergency Generator", "Small Delivery"),
    numeric_vars = c("Compartment Capacity")
  ),
  environmental_sites = list(
    source = "ust_all-tn-environmental-sites.xlsx",
    id_vars = c("Facilityid", "Sitenumber"),
    name_vars = c("Facilityname", "Company", "Ownercontact", "Caccontact", "Caccompany"),
    address_vars = c(
      "Facilityaddress1", "Facilityaddress2", "Facilitycity",
      "Facilityzip", "Facilitycounty"
    ),
    date_vars = c("Discoverydate"),
    categorical_vars = c(
      "Casemanager", "Section", "Casedescription", "Cause",
      "Currentstatus", "Productreleased", "Contacttitle", "Howdiscovered"
    )
  ),
  facilities = list(
    source = "Facilities.csv (EPA National)",
    id_vars = c("OBJECTID", "Facility_ID"),
    geo_vars = c("Latitude", "Longitude"),
    categorical_vars = c("County", "Facility_Status")
  ),
  zip_crosswalk = list(
    source = "uszips.csv",
    id_vars = c("zip"),
    geo_vars = c("lat", "lng", "county_fips", "county_name"),
    categorical_vars = c("city", "state_id", "state_name")
  )
)

# =============================================================================
# TEXAS
# Sources: TCEQ fixed-width files (pst_*.txt) + TX_LUST.csv
# NOTE: Texas uses fixed-width format with position specifications
# =============================================================================
TX_vars <- list(
  lust = list(
    source = "TX_LUST.csv",
    id_vars = c("facility_id", "LUST_id"),
    date_vars = c("report_date", "nfa_date")
  ),
  # Texas PST files are fixed-width; variable names from your existing tx_fr_panel.R
  # These are comprehensive lists based on TCEQ data specifications
  pst_fac = list(
    source = "pst_fac.txt (fixed-width)",
    id_vars = c("FACILITY_ID", "TRACKING_NUMBER"),
    name_vars = c("FACILITY_NAME", "DBA_NAME"),
    address_vars = c(
      "SITE_ADDRESS", "SITE_CITY", "SITE_COUNTY", "SITE_ZIP",
      "MAIL_ADDRESS", "MAIL_CITY", "MAIL_STATE", "MAIL_ZIP"
    ),
    geo_vars = c("LATITUDE", "LONGITUDE"),
    date_vars = c("REG_DATE", "STATUS_DATE"),
    categorical_vars = c("FACILITY_TYPE", "FACILITY_STATUS", "UST_FR_REQUIRED")
  ),
  pst_ust = list(
    source = "pst_ust.txt (fixed-width, 192 cols)",
    id_vars = c("UST_ID", "FACILITY_ID"),
    date_vars = c("INSTALL_DATE", "REG_DATE", "CLOSURE_DATE", "STATUS_DATE"),
    categorical_vars = c(
      "TANK_STATUS", "TANK_SINGLE", "TANK_DOUBLE", "TANK_MATERIAL",
      "TANK_CP_TYPE", "TANK_UPGRADE"
    ),
    numeric_vars = c("CAPACITY"),
    # Detection method flags (18 total: DET_C_*, DET_P_*)
    detection_flags = paste0("DET_", rep(c("C_", "P_"), each = 9), 
                             rep(c("ATG", "MANUAL", "INTERSTITIAL", "VAPOR", "GW", 
                                   "SIR", "TIGHTNESS", "OTHER", "NONE"), 2))
  ),
  pst_ust_comprt = list(
    source = "pst_ust_comprt.txt (fixed-width)",
    id_vars = c("UST_ID", "COMPARTMENT_ID"),
    categorical_vars = c("SUBSTANCE_CODE", "SUBSTANCE_DESC"),
    numeric_vars = c("COMPARTMENT_CAPACITY")
  ),
  pst_fin_assur = list(
    source = "pst_fin_assur.txt (fixed-width)",
    id_vars = c("FIN_ASSUR_ID", "FACILITY_ID"),
    date_vars = c("EFFECTIVE_DATE", "EXPIRATION_DATE"),
    categorical_vars = c("DETAIL_TYPE", "CATEGORY", "ISSUER_NAME"),
    numeric_vars = c("COVER_OCC", "COVER_AGG")
  ),
  pst_own = list(
    source = "pst_own.txt (fixed-width)",
    id_vars = c("OWNER_ID", "FACILITY_ID"),
    name_vars = c("OWNER_NAME"),
    date_vars = c("START_DATE", "END_DATE"),
    categorical_vars = c("OWNER_TYPE")
  ),
  pst_opr = list(
    source = "pst_opr.txt (fixed-width)",
    id_vars = c("OPERATOR_ID", "FACILITY_ID"),
    name_vars = c("OPERATOR_NAME"),
    date_vars = c("START_DATE", "END_DATE")
  )
)

# =============================================================================
# ALASKA (EPA National LUST file only)
# =============================================================================
AK_vars <- list(
  lust = list(
    source = "LUST.csv (state-provided)",
    id_vars = c("OBJECTID", "Facility_ID", "LUST_ID"),
    name_vars = c("Name"),
    address_vars = c("Address", "City", "County", "Zip_Code", "State"),
    geo_vars = c("Latitude", "Longitude", "Coordinate_Source", "Address_Match_Type"),
    date_vars = c("Reported_Date"),
    categorical_vars = c(
      "Status", "Substance", "LandUse",
      "Within_SPA", "SPA_Water_Type", "SPA_Facility_Type", "SPA_HUC12",
      "Within_WHPA", "WHPA_Water_Type", "WHPA_Facility_Type", "WHPA_HUC12",
      "Within_100yr_Floodplain"
    ),
    numeric_vars = c("Population_within_1500ft", "DomesticWells_within_1500ft"),
    flag_vars = c("Open_", "Closed")
  )
)

# =============================================================================
# MASTER REGISTRY OF ALL STATES
# =============================================================================
STATE_VAR_REGISTRY <- list(
  AL = AL_vars,
  AR = AR_vars,
  CO = CO_vars,
  LA = LA_vars,
  ME = ME_vars,
  MI = MI_vars,
  NJ = NJ_vars,
  NM = NM_vars,
  OK = OK_vars,
  TN = TN_vars,
  TX = TX_vars,
  AK = AK_vars
)

# =============================================================================
# HELPER FUNCTIONS FOR DATA QUALITY ASSESSMENT
# =============================================================================

#' Get all variables for a state (flattened)
#' @param state_abbr Two-letter state abbreviation
#' @return Character vector of all variable names
get_all_vars <- function(state_abbr) {
  state_list <- STATE_VAR_REGISTRY[[state_abbr]]
  if (is.null(state_list)) stop(paste("State not found:", state_abbr))
  
  all_vars <- character()
  for (source_name in names(state_list)) {
    source_data <- state_list[[source_name]]
    for (var_type in names(source_data)) {
      if (var_type != "source" && is.character(source_data[[var_type]])) {
        all_vars <- c(all_vars, source_data[[var_type]])
      }
    }
  }
  unique(all_vars)
}

#' Get variables by type for a state
#' @param state_abbr Two-letter state abbreviation
#' @param var_type Variable type (e.g., "date_vars", "categorical_vars")
#' @return Named list by source
get_vars_by_type <- function(state_abbr, var_type) {
  state_list <- STATE_VAR_REGISTRY[[state_abbr]]
  if (is.null(state_list)) stop(paste("State not found:", state_abbr))
  
  result <- list()
  for (source_name in names(state_list)) {
    source_data <- state_list[[source_name]]
    if (!is.null(source_data[[var_type]])) {
      result[[source_name]] <- source_data[[var_type]]
    }
  }
  result
}

#' Get date variables for type conversion testing
get_date_vars <- function(state_abbr) get_vars_by_type(state_abbr, "date_vars")

#' Get categorical variables for frequency tables
get_categorical_vars <- function(state_abbr) get_vars_by_type(state_abbr, "categorical_vars")

#' Get flag/binary variables
get_flag_vars <- function(state_abbr) get_vars_by_type(state_abbr, "flag_vars")

#' Get ID variables for uniqueness checks
get_id_vars <- function(state_abbr) get_vars_by_type(state_abbr, "id_vars")

#' Get numeric variables for range/distribution checks
get_numeric_vars <- function(state_abbr) get_vars_by_type(state_abbr, "numeric_vars")

#' Print summary of state variable registry
print_state_summary <- function(state_abbr = NULL) {
  states <- if (is.null(state_abbr)) names(STATE_VAR_REGISTRY) else state_abbr
  
  for (st in states) {
    cat("\n", strrep("=", 60), "\n")
    cat("STATE:", st, "\n")
    cat(strrep("=", 60), "\n")
    
    state_list <- STATE_VAR_REGISTRY[[st]]
    for (source_name in names(state_list)) {
      source_data <- state_list[[source_name]]
      n_vars <- sum(sapply(source_data[names(source_data) != "source"], length))
      cat(sprintf("  %s: %d variables\n", source_name, n_vars))
      if (!is.null(source_data$source)) {
        cat(sprintf("    Source: %s\n", source_data$source))
      }
    }
  }
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================
# print_state_summary()                    # All states
# print_state_summary("AR")                # Arkansas only
# get_date_vars("AR")                      # All date vars by source
# get_categorical_vars("TX")               # All categorical vars for Texas
# get_all_vars("CO")                       # Flat list of all Colorado vars

cat("\nState variable registry loaded successfully.\n")
cat("Available states:", paste(names(STATE_VAR_REGISTRY), collapse = ", "), "\n")
cat("Use print_state_summary() to view details.\n")
