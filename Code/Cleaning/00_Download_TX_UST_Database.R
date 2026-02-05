###############################################################################
# 00_download_texas.R
# ===================
# Texas UST Data Pipeline - Stage 0: Ingestion & Structural Audit
#
# Purpose:
#   Download raw PST (Petroleum Storage Tank) text files from TCEQ
#   and validate structural integrity against official data dictionary.
#
# Data Source:
#   Texas Commission on Environmental Quality (TCEQ)
#   https://www.tceq.texas.gov/assets/public/admin/data/docs/
#
# Output:
#   Raw .txt files saved to: Data/Raw/state_databases/Texas/
#   Audit report printed to console
#
# Author: UST Research Pipeline
# Date: 2026-02
#
# CONSTRAINT: This script does NOT perform any data cleaning or transformation.
#             It only downloads and validates structural integrity.
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 0: Setup & Configuration
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

library(data.table)
library(here)

# ── Path Configuration ───────────────────────────────────────────────────────
BASE_URL   <- "https://www.tceq.texas.gov/assets/public/admin/data/docs"
OUTPUT_DIR <- here("Data", "Raw", "state_databases", "Texas")

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created output directory: ", OUTPUT_DIR)
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1: Data Dictionary Specifications
# ══════════════════════════════════════════════════════════════════════════════
# Source: TCEQ PST Data Specifications (petroleum-storage-tank-data-specifications)
# These specifications define the fixed-width layout for each file.

# ── Helper function to create file specifications ────────────────────────────
create_file_spec <- function(filename, description, record_length, col_widths, col_names) {
  # Validate that widths sum to record length
  actual_sum <- sum(col_widths)
  if (actual_sum != record_length) {
    stop(sprintf("%s: Column widths sum to %d, expected %d", 
                 filename, actual_sum, record_length))
  }
  if (length(col_widths) != length(col_names)) {
    stop(sprintf("%s: %d widths but %d names", 
                 filename, length(col_widths), length(col_names)))
  }
  
  list(
    filename      = filename,
    description   = description,
    record_length = record_length,
    col_widths    = col_widths,
    col_names     = col_names,
    n_cols        = length(col_names)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# FILE SPECIFICATIONS (Updated based on actual server filenames)
# ══════════════════════════════════════════════════════════════════════════════

# ── PST_FAC.TXT: Facility Master File (1379 bytes) ───────────────────────────
PST_FAC_SPEC <- create_file_spec(
  filename      = "pst_fac.txt",
  description   = "PST Facility Master - Static facility-level attributes",
  record_length = 1379,
  col_widths    = c(
    8, 15, 6, 60, 30, 10, 30, 1, 1, 1,      # 1-10
    4, 4, 50, 30, 2, 5, 4, 256, 35, 35,     # 11-20
    2, 5, 15, 15, 28, 60, 100, 50, 50, 30,  # 21-30
    2, 5, 4, 3, 7, 5, 3, 7, 5, 50,          # 31-40
    1, 10, 10, 15, 15, 28, 60, 35, 100, 1,  # 41-50
    10, 1, 30, 30                           # 51-54
  ),
  col_names     = c(
    "FACILITY_ID", "ADD_ID", "AI", "FACILITY_NAME", "FACILITY_TYPE",
    "FACILITY_BEGIN_DATE", "FACILITY_STATUS", "FACILITY_EXEMPT_STATUS",
    "RECORDS_OFF_SITE", "UST_FR_REQUIRED", "NUM_ACTIVE_USTS", "NUM_ACTIVE_ASTS",
    "SITE_ADDRESS_DELIVERY", "SITE_CITY", "SITE_STATE_CODE", "SITE_ZIP",
    "SITE_ZIP_EXT", "SITE_LOCATION_DESC", "SITE_NEAREST_CITY", "SITE_COUNTY",
    "SITE_REGION", "SITE_LOCATION_ZIP", "CONTACT_FN", "CONTACT_MI", "CONTACT_LN",
    "CONTACT_TITLE", "CONTACT_ORG", "CONTACT_ADDR_DELIV", "CONTACT_ADDR_INTDELIV",
    "CONTACT_ADDR_CITY", "CONTACT_ADDR_STATE", "CONTACT_ADDR_ZIP",
    "CONTACT_ADDR_ZIP_EXT", "CONTACT_PHONE_AC", "CONTACT_PHONE_NUM",
    "CONTACT_PHONE_EXT", "CONTACT_FAX_AC", "CONTACT_FAX_NUM", "CONTACT_FAX_EXT",
    "CONTACT_EMAIL", "CONTACT_ADDR_DELIVERABLE", "APP_REC_DATE", "SIG_DATE",
    "SIG_FN", "SIG_MI", "SIG_LN", "SIG_TITLE", "SIG_ROLE", "SIG_COMPANY",
    "ENFORCEMENT_ACTION", "ENFORCEMENT_ACTION_DATE",
    "FACILITY_NOT_INSPECTABLE", "NOT_INSPECTABLE_REASON_1",
    "NOT_INSPECTABLE_REASON_2"
  )
)

# ── PST_FIN_ASSUR.TXT: Financial Assurance Records (278 bytes) ───────────────
PST_FIN_ASSUR_SPEC <- create_file_spec(
  filename      = "pst_fin_assur.txt",
  description   = "Financial Assurance/Insurance Contracts",
  record_length = 278,
  col_widths    = c(
    8,   # FIN_ASSUR_ID
    8,   # FACILITY_ID (padded)
    6,   # AI
    10,  # FORM_REC_DATE
    30,  # MECH_TYPE
    30,  # MECH_TYPE_OTHER
    1,   # MULTI_MECH_TYPES
    50,  # ISSUER_NAME
    5,   # ISSUER_PHONE_CTRY
    3,   # ISSUER_PHONE_AREA
    7,   # ISSUER_PHONE_NUM
    5,   # ISSUER_PHONE_EXT
    30,  # POLICY_MECH_NUM
    10,  # COVER_EFF (Effective Date)
    10,  # COVER_EXP (Expiration Date)
    30,  # COVER_OCC (Per Occurrence)
    30,  # COVER_AGG (Annual Aggregate)
    1,   # PREMIUM_PREPAID
    1,   # FP_CORR_MET
    1,   # TP_FA_MET
    1,   # PROOF_OF_FA
    1    # MEETS_FLAG
  ),
  col_names     = c(
    "FIN_ASSUR_ID", "FACILITY_ID_PAD", "FACILITY_AI", "FORM_REC_DATE",
    "MECH_TYPE", "MECH_TYPE_OTHER", "MULTI_MECH_TYPES", "ISSUER_NAME",
    "ISSUER_PHONE_CTRY", "ISSUER_PHONE_AREA", "ISSUER_PHONE_NUM",
    "ISSUER_PHONE_EXT", "POLICY_MECH_NUM", "COVER_EFF", "COVER_EXP",
    "COVER_OCC", "COVER_AGG", "PREMIUM_PREPAID", "FP_CORR_MET",
    "TP_FA_MET", "PROOF_OF_FA", "MEETS_FLAG"
  )
)

# ── PST_UST.TXT: Underground Storage Tank Records (201 bytes) ────────────────
# Correction: Mapped 16 distinct corrosion flags (bytes 169-184) to prevent 
# misalignment of compliance data.
PST_UST_SPEC <- create_file_spec(
  filename      = "pst_ust.txt",
  description   = "UST Tank-Level Attributes and Construction Details",
  record_length = 201,
  col_widths    = c(
    # Core fields (Bytes 1-143)
    8, 8, 6, 10, 2, 10, 10, 8, 30, 10, 1, 30, 10, 
    # Tank/Pipe Design & Containment (Bytes 144-153)
    1, 1, 1, 1,      # Tank/Pipe Single/Double
    1, 1, 1,         # Tank Ext Cont (Jacket, Syn, Vault)
    1, 1, 1,         # Pipe Ext Cont (Jacket, Syn, Vault)
    # Materials & Hardware (Bytes 154-168)
    1,               # Pipe Type
    1, 1, 1, 1, 1, 1,# Tank Mat (Steel, FRP, Comp, Conc, Jacketed, Coated)
    1, 1, 1, 1, 1,   # Pipe Mat (Steel, FRP, Conc, Jacketed, Flex)
    1, 1, 1,         # Pipe Hardware (Shear, Swing, Flex)
    # Corrosion Protection - TANK (Bytes 169-176)
    1, 1, 1, 1, 1, 1, 1, 1,
    # Corrosion Protection - PIPING (Bytes 177-184)
    1, 1, 1, 1, 1, 1, 1, 1,
    # Compliance & Variance (Bytes 185-188)
    1, 1,            # Tank/Pipe CP Compliance
    1, 1,            # Tank/Pipe CP Variance
    # Operational Status & Signature (Bytes 189-201)
    1,               # TOS (Temp Out of Service) Compliance
    1,               # Tech Compliance
    1,               # Tank Tested
    10               # Install Sig Date
  ),
  col_names     = c(
    # Core fields
    "UST_ID", "FACILITY_ID_PAD", "AI", "TANK_ID", "COMPARTS",
    "INSTALL_DATE", "REG_DATE", "CAPACITY", "STATUS", "STATUS_DATE",
    "EMPTY", "REG_STATUS", "TANK_INT_PROT_DATE", 
    # Tank/Pipe Design
    "TANK_DES_SINGLE", "TANK_DES_DOUBLE", "PIP_DES_SINGLE", "PIP_DES_DOUBLE",
    # External Containment
    "EXT_CONT_TANK_JACKET", "EXT_CONT_TANK_SYN", "EXT_CONT_TANK_VAULT",
    "EXT_CONT_PIP_JACKET", "EXT_CONT_PIP_SYN", "EXT_CONT_PIP_VAULT",
    # Pipe Type & Materials
    "PIPE_TYPE",
    "TMAT_STEEL", "TMAT_FRP", "TMAT_COMPOSITE", "TMAT_CONCRETE", "TMAT_JACKETED", "TMAT_COATED",
    "PMAT_STEEL", "PMAT_FRP", "PMAT_CONCRETE", "PMAT_JACKETED", "PMAT_FLEX",
    "PHARD_SHEAR", "PHARD_SWING", "PHARD_FLEX",
    # Corrosion Protection - TANK (169-176)
    "CP_TANK_DIELECTRIC", "CP_TANK_CATHODIC_FAC", "CP_TANK_CATHODIC_FIELD", 
    "CP_TANK_COMPOSITE", "CP_TANK_COATED", "CP_TANK_FRP", 
    "CP_TANK_JACKETED", "CP_TANK_NOT_REQ",
    # Corrosion Protection - PIPING (177-184)
    "CP_PIP_DIELECTRIC", "CP_PIP_CATHODIC_FAC", "CP_PIP_CATHODIC_FIELD",
    "CP_PIP_FRP", "CP_PIP_FLEX", "CP_PIP_ISOLATED", 
    "CP_PIP_DUAL", "CP_PIP_NOT_REQ",
    # Compliance & Variance
    "COMPLY_CP_TANK", "COMPLY_CP_PIPE",
    "VAR_CP_TANK", "VAR_CP_PIPE",
    # Final Status
    "COMPLY_TOS",       # Byte 189 (Temporarily Out of Service)
    "COMPLY_TECH",      # Byte 190
    "TANK_TESTED_FLAG", # Byte 191
    "INSTALL_SIG_DATE"
  )
)

# ── PST_UST_COMPRT.TXT: UST Compartment Records (201 bytes) ──────────────────
PST_UST_COMPRT_SPEC <- create_file_spec(
  filename      = "pst_ust_comprt.txt",
  description   = "UST Compartment - Substance and Detection Methods",
  record_length = 201,
  col_widths    = c(
    8,    # UST_COMPRT_ID
    8,    # UST_ID
    6,    # FACILITY_ID
    10,   # TANK_ID
    1,    # COMPRT_ID
    8,    # CAPACITY
    30,   # SUBSTANCE_STORED_1
    30,   # SUBSTANCE_STORED_2
    30,   # SUBSTANCE_STORED_3
    rep(1, 30),  # 30 detection/compliance/spill flags
    30,   # STAGE1_VAPOR_RECOVERY
    10    # STAGE1_INSTALL_DATE
  ),
  col_names     = c(
    "UST_COMPRT_ID", "UST_ID", "FACILITY_ID", "TANK_ID", "COMPRT_ID",
    "CAPACITY",
    "SUBSTANCE_STORED_1", "SUBSTANCE_STORED_2", "SUBSTANCE_STORED_3",
    # Compartment release detection (8)
    "DET_C_VAPOR", "DET_C_GW", "DET_C_SEC_CONT", "DET_C_ATG",
    "DET_C_INTERSTITIAL", "DET_C_MANUAL_WEEK", "DET_C_MANUAL_MONTH", "DET_C_SIR",
    # Piping release detection (10)
    "DET_P_VAPOR", "DET_P_GW", "DET_P_SEC_CONT", "DET_P_INTERSTITIAL",
    "DET_P_TT_MONTH", "DET_P_TT_ANNUAL", "DET_P_TT_TRIEN", "DET_P_LLD",
    "DET_P_SIR", "DET_P_SUCTION_EXEMPT",
    # Spill & overfill prevention (6)
    "SPILL_TIGHT_FILL", "SPILL_FACTORY_SPILL", "SPILL_SHUTOFF_VALVE",
    "SPILL_FLOW_RESTRICT", "SPILL_ALARM", "SPILL_NA",
    # Compliance flags (3)
    "COMPRT_DET_COMPLY", "PIP_DET_COMPLY", "SPILL_COMPLY",
    # Variance flags (3)
    "COMPRT_DET_VARIANCE", "PIP_DET_VARIANCE", "SPILL_VARIANCE",
    # Stage 1 vapor recovery
    "STAGE1_VAPOR_RECOVERY", "STAGE1_INSTALL_DATE"
  )
)

# ── PST_OWN.TXT: Owner CN Records (682 bytes) ────────────────────────────────
PST_OWN_SPEC <- create_file_spec(
  filename      = "pst_own.txt",
  description   = "Owner CN - Facility ownership information",
  record_length = 682,
  col_widths    = c(
    15, 15, 6, 11, 100, 15, 15, 10, 2, 15,  # 1-10: PRINC_ID through STATE_TAX_ID
    10, 15, 15, 28, 60, 100,                 # 11-16: CONTACT_ROLE through CONTACT_ORG
    50, 50, 30, 2, 5, 4, 15, 3,             # 17-24: Mail address fields
    5, 3, 7, 5, 5, 3, 7, 5, 50, 1           # 25-34: Phone/fax/email fields
  ),
  col_names     = c(
    "PRINC_ID", "ADD_ID", "FACILITY_ID", "OWNER_CN",
    "OWNER_NAME_CO_LAST", "OWNER_NAME_FIRST", "OWNER_NAME_MIDDLE",
    "OWNER_EFF_BEGIN_DT", "OWNER_TYPE", "STATE_TAX_ID",
    "CONTACT_ROLE", "CONTACT_FIRST_NM", "CONTACT_MIDDLE_NM", "CONTACT_LAST_NM",
    "CONTACT_TITLE", "CONTACT_ORG_NM",
    "MAIL_ADDR_DELIVERY", "MAIL_ADDR_INTERNAL", "MAIL_CITY", "MAIL_STATE",
    "MAIL_ZIP", "MAIL_ZIP_PLUS4", "MAIL_FOREIGN_POSTAL", "MAIL_COUNTRY",
    "PHONE_COUNTRY", "PHONE_AREA", "PHONE_NUM", "PHONE_EXT",
    "FAX_COUNTRY", "FAX_AREA", "FAX_NUM", "FAX_EXT",
    "EMAIL", "ADDR_DELIVERABLE_FLAG"
  )
)

# ── PST_OPR.TXT: Operator CN Records (667 bytes) ─────────────────────────────
PST_OPR_SPEC <- create_file_spec(
  filename      = "pst_opr.txt",
  description   = "Operator CN - Facility operator information",
  record_length = 667,
  col_widths    = c(
    15, 15, 6, 11, 100, 15, 15, 10, 2,      # 1-9: PRINC_ID through OPERATOR_TYPE
    10, 15, 15, 28, 60, 100,                 # 10-15: CONTACT_ROLE through CONTACT_ORG
    50, 50, 30, 2, 5, 4, 15, 3,             # 16-23: Mail address fields
    5, 3, 7, 5, 5, 3, 7, 5, 50, 1           # 24-33: Phone/fax/email fields
  ),
  col_names     = c(
    "PRINC_ID", "ADD_ID", "FACILITY_ID", "OPERATOR_CN",
    "OPERATOR_NAME_CO_LAST", "OPERATOR_NAME_FIRST", "OPERATOR_NAME_MIDDLE",
    "OPERATOR_EFF_BEGIN_DT", "OPERATOR_TYPE",
    "CONTACT_ROLE", "CONTACT_FIRST_NM", "CONTACT_MIDDLE_NM", "CONTACT_LAST_NM",
    "CONTACT_TITLE", "CONTACT_ORG_NM",
    "MAIL_ADDR_DELIVERY", "MAIL_ADDR_INTERNAL", "MAIL_CITY", "MAIL_STATE",
    "MAIL_ZIP", "MAIL_ZIP_PLUS4", "MAIL_FOREIGN_POSTAL", "MAIL_COUNTRY",
    "PHONE_COUNTRY", "PHONE_AREA", "PHONE_NUM", "PHONE_EXT",
    "FAX_COUNTRY", "FAX_AREA", "FAX_NUM", "FAX_EXT",
    "EMAIL", "ADDR_DELIVERABLE_FLAG"
  )
)

# ── PST_SELF_CERT.TXT: Self-Certification Records (192 bytes) ────────────────
PST_SELF_CERT_SPEC <- create_file_spec(
  filename      = "pst_self_cert.txt",
  description   = "Self-Certification - Annual compliance attestations",
  record_length = 192,
  col_widths    = c(
    8, 8, 6, 10, 50, 30, 30, 30,  # ID fields and signature info
    1, 1, 1, 1,                    # Certification flags (REG, FEE, FA, TECH)
    10,                            # DEL_CERT_EXP
    1, 1, 1, 1, 1, 1               # Additional compliance indicators
  ),
  col_names     = c(
    "SC_ID", "FACILITY_ID_PAD", "AI", "SIGN_DATE",
    "SIGN_NAME", "SIGN_TITLE", "SIGN_TYPE_ROLE", "FILING_STATUS",
    "REG_FLAG", "FEE_FLAG", "FA_FLAG", "TECH_FLAG",
    "DEL_CERT_EXP",
    "REPORTING_METHOD", "TANK_CP_COMPLY", "PIP_CP_COMPLY",
    "COMPRT_DET_COMPLY", "PIP_DET_COMPLY", "SPILL_COMPLY"
  )
)

# ── PST_FAC_BILL_CONT.TXT: Facility Billing Contacts (556 bytes) ─────────────
# Filename corrected from pst_bill_contacts.txt to pst_fac_bill_cont.txt
PST_BILL_CONTACTS_SPEC <- create_file_spec(
  filename      = "pst_fac_bill_cont.txt", 
  description   = "Facility Billing Contacts - Invoice recipients",
  record_length = 556,
  col_widths    = c(
    8, 15, 15, 6, 60, 10, 1, 1,      # IDs & AR info (1-116)
    15, 15, 28, 60, 100,             # Contact Name/Org (117-334)
    50, 50, 30, 2, 5, 4,             # Address (335-475)
    3, 7, 5,                         # Phone (476-490)
    3, 7, 5,                         # Fax (491-505)
    50, 1                            # Email & Flag (506-556)
  ),
  col_names     = c(
    "FACILITY_ID", "ADD_ID", "PRINC_ID", "AI", "FACILITY_NAME",
    "AR_NUM", "AR_SUFFIX_UST", "AR_SUFFIX_AST",
    "CONTACT_FN", "CONTACT_MN", "CONTACT_LN", "CONTACT_TITLE", "CONTACT_ORG",
    "ADDR_DELIV", "ADDR_INT_DELIV", "ADDR_CITY", "ADDR_STATE", "ADDR_ZIP", "ADDR_ZIP_EXT",
    "PHONE_AC", "PHONE_NUM", "PHONE_EXT",
    "FAX_AC", "FAX_NUM", "FAX_EXT",
    "EMAIL", "ADDR_DELIVERABLE"
  )
)

# ── PST_SELF_CERT_UST.TXT: Self-Certification USTs (49 bytes) ────────────────
# Filename corrected from pst_self_cert_usts.txt to pst_self_cert_ust.txt
PST_SELF_CERT_USTS_SPEC <- create_file_spec(
  filename      = "pst_self_cert_ust.txt",
  description   = "Self-Certification UST - Tank-level linkage",
  record_length = 49,
  col_widths    = c(
    8, 8, 8, 8, 6, 10, 1
  ),
  col_names     = c(
    "SELF_CERT_COMPRT_ID", "SELF_CERT_ID", "UST_COMPRT_ID", "UST_ID",
    "FACILITY_ID_AI", "TANK_ID", "COMPARTMENT_ID"
  )
)

# ── PST_CONST_NOTIF.TXT: Construction Notification (843 bytes) ───────────────
PST_CONST_NOTIF_SPEC <- create_file_spec(
  filename      = "pst_const_notif.txt",
  description   = "Construction Notification - Planned UST/AST work",
  record_length = 843,
  col_widths    = c(
    8, 8, 6, 10, 10,                 # IDs & Dates (1-42)
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    # Work Type Flags (43-52)
    15, 1, 1, 10, 10,                # Tracking & Filing (53-89)
    15, 15, 28, 100, 60, 35,         # Signature Block (90-342)
    203, 25, 23, 250                 # Owner info & Desc (343-843)
  ),
  col_names     = c(
    "NOC_ID", "FACILITY_ID", "AI", "APP_REC_DATE", "SCHED_CONST_DATE",
    "WORK_UST_IMPROVE", "WORK_UST_INSTALL", "WORK_UST_REMOVE", "WORK_UST_REPAIR",
    "WORK_UST_RETURN", "WORK_UST_REPLACE", "WORK_UST_ABANDON", "WORK_UST_STAGE1",
    "WORK_AST_INSTALL", "WORK_AST_STAGE1",
    "HIST_TRACK_NUM", "WAIVER_FLAG", "LATE_FLAG", "FORM_REC_DATE", "SIG_DATE",
    "SIG_FN", "SIG_MN", "SIG_LN", "SIG_COMPANY", "SIG_TITLE", "SIG_ROLE",
    "OWNER_NAMES_CONST", "OWNER_CNS_CONST", "OWNER_ARS_CONST", "GEN_DESC"
  )
)

# ── PST_CNTRCTR_INSTL_CONSUL.TXT: Contractor/Consultant/Installer (447 bytes) ─
# Filename corrected from pst_cont_cons_inst.txt to pst_cntrctr_instl_consul.txt
PST_CONT_CONS_INST_SPEC <- create_file_spec(
  filename      = "pst_cntrctr_instl_consul.txt",
  description   = "Contractor/Consultant - Parties performing work",
  record_length = 447,
  col_widths    = c(
    8, 8, 8, 6, 10, 9, 100,          # IDs & Company (1-149)
    15, 15, 28,                      # Rep Name (150-207)
    50, 50, 30, 2, 5, 15, 3,         # Address (208-362)
    5, 3, 7, 5,                      # Phone (363-382)
    5, 3, 7,                         # Fax (383-397)
    50                               # Email (398-447)
  ),
  col_names     = c(
    "CCI_ID", "UST_ID", "NOC_ID", "AI", "CONTACT_TYPE", "CRP_ILP_NUM", "COMPANY_NAME",
    "REP_FN", "REP_MN", "REP_LN",
    "ADDR_DELIV", "ADDR_INT_DELIV", "ADDR_CITY", "ADDR_STATE", "ADDR_ZIP",
    "ADDR_FOREIGN_POSTAL", "ADDR_COUNTRY",
    "PHONE_COUNTRY", "PHONE_AC", "PHONE_NUM", "PHONE_EXT",
    "FAX_COUNTRY", "FAX_AC", "FAX_NUM",
    "EMAIL"
  )
)

# ── PST_AST.TXT: Aboveground Storage Tank (270 bytes) ────────────────────────
PST_AST_SPEC <- create_file_spec(
  filename      = "pst_ast.txt",
  description   = "Aboveground Storage Tank - AST attributes",
  record_length = 270,
  col_widths    = c(
    8, 8, 6, 10, 1, 10, 10, 30, 10, 30, 8, # IDs & Status (1-131)
    30, 30, 30,                            # Substances (132-221)
    1, 1, 1, 1, 1,                         # Material Flags (222-226)
    1, 1, 1, 1,                            # Containment Flags (227-230)
    30, 10                                 # Stage I (231-270)
  ),
  col_names     = c(
    "AST_ID", "FACILITY_ID", "AI", "TANK_ID", "MULTI_COMPARTMENT",
    "INSTALL_DATE", "REG_DATE", "STATUS", "STATUS_DATE", "REG_STATUS", "CAPACITY",
    "SUBSTANCE_1", "SUBSTANCE_2", "SUBSTANCE_3",
    "MAT_STEEL", "MAT_FIBERGLASS", "MAT_ALUM", "MAT_CORR_METAL", "MAT_CONCRETE",
    "CONT_EARTHEN", "CONT_LINER", "CONT_CONCRETE", "CONT_NONE",
    "STAGE1_VAPOR_RECOVERY", "STAGE1_INSTALL_DATE"
  )
)

# ── Updated File Specification List ──────────────────────────────────────────
FILE_SPECS <- list(
  pst_fac             = PST_FAC_SPEC,
  pst_fin_assur       = PST_FIN_ASSUR_SPEC,
  pst_ust             = PST_UST_SPEC,
  pst_ust_comprt      = PST_UST_COMPRT_SPEC,
  pst_own             = PST_OWN_SPEC,
  pst_opr             = PST_OPR_SPEC,
  pst_self_cert       = PST_SELF_CERT_SPEC,
  pst_bill_contacts   = PST_BILL_CONTACTS_SPEC,
  pst_self_cert_usts  = PST_SELF_CERT_USTS_SPEC,
  pst_const_notif     = PST_CONST_NOTIF_SPEC,
  pst_cont_cons_inst  = PST_CONT_CONS_INST_SPEC,
  pst_ast             = PST_AST_SPEC
)


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2: Download Functions
# ══════════════════════════════════════════════════════════════════════════════

#' Download a single file from TCEQ
#'
#' @param spec File specification list
#' @param output_dir Directory to save the file
#' @param timeout Download timeout in seconds
#' @return List with status, path, and any error message
download_pst_file <- function(spec, output_dir, timeout = 600) {
  url       <- paste0(BASE_URL, "/", spec$filename)
  dest_path <- file.path(output_dir, spec$filename)
  
  message("⇣  Downloading: ", spec$filename)
  
  # Set timeout
 old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)
  
  result <- tryCatch({
    download.file(url, dest_path, method = "libcurl", quiet = TRUE)
    list(
      success  = TRUE,
      filename = spec$filename,
      path     = dest_path,
      error    = NULL
    )
  }, error = function(e) {
    list(
      success  = FALSE,
      filename = spec$filename,
      path     = NULL,
      error    = conditionMessage(e)
    )
  }, warning = function(w) {
    # Check if download.file generated a non-zero exit status warning
    list(
        success = FALSE,
        filename = spec$filename,
        path = NULL,
        error = conditionMessage(w)
    )
  })
  
  if (result$success) {
    message("✓  Downloaded: ", spec$filename)
  } else {
    message("✗  FAILED: ", spec$filename, " - ", result$error)
  }
  
  return(result)
}

#' Download all PST files
#'
#' @param specs List of file specifications
#' @param output_dir Directory to save files
#' @return Data.table with download results
download_all_pst_files <- function(specs, output_dir) {
  results <- lapply(specs, download_pst_file, output_dir = output_dir)
  
  # Convert to data.table
  results_dt <- rbindlist(lapply(results, function(r) {
    data.table(
      filename = r$filename,
      success  = r$success,
      path     = ifelse(is.null(r$path), NA_character_, r$path),
      error    = ifelse(is.null(r$error), NA_character_, r$error)
    )
  }))
  
  return(results_dt)
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3: Audit Functions
# ══════════════════════════════════════════════════════════════════════════════

#' Audit a single downloaded file against its specification
#'
#' @param spec File specification list
#' @param file_path Path to the downloaded file
#' @return List with audit results
audit_file <- function(spec, file_path) {
  if (!file.exists(file_path)) {
    return(list(
      filename     = spec$filename,
      exists       = FALSE,
      file_size    = NA,
      record_count = NA,
      issues       = "File does not exist"
    ))
  }
  
  # Read raw lines
  raw_lines <- readLines(file_path, warn = FALSE)
  record_count <- length(raw_lines)
  file_size <- file.info(file_path)$size
  
  issues <- character()
  
  # Check 1: Line lengths
  line_lengths <- nchar(raw_lines)
  expected_length <- spec$record_length
  
  incorrect_lengths <- which(line_lengths != expected_length)
  if (length(incorrect_lengths) > 0) {
    # Sample some bad lines for the report
    sample_bad <- head(incorrect_lengths, 5)
    bad_lengths <- unique(line_lengths[sample_bad])
    issues <- c(issues, sprintf(
      "Line length mismatch: Expected %d, found %d lines with different lengths (e.g., %s)",
      expected_length, length(incorrect_lengths), paste(bad_lengths, collapse = ", ")
    ))
  }
  
  # Check 2: Verify column parsing (test first 100 lines)
  test_lines <- head(raw_lines, 100)
  
  # Try to parse using the specified widths
  parse_result <- tryCatch({
    # Calculate cumulative positions
    widths <- spec$col_widths
    starts <- cumsum(c(1, widths[-length(widths)]))
    ends   <- cumsum(widths)
    
    # Parse first line as a test
    if (length(test_lines) > 0) {
      test_row <- test_lines[1]
      parsed_cols <- mapply(
        function(s, e) substr(test_row, s, e),
        starts, ends,
        SIMPLIFY = TRUE
      )
      
      if (length(parsed_cols) != spec$n_cols) {
        return(list(success = FALSE, 
                    msg = sprintf("Parsed %d columns, expected %d",
                                  length(parsed_cols), spec$n_cols)))
      }
      list(success = TRUE, msg = NULL)
    } else {
      list(success = TRUE, msg = "Empty file")
    }
  }, error = function(e) {
    list(success = FALSE, msg = conditionMessage(e))
  })
  
  if (!parse_result$success) {
    issues <- c(issues, sprintf("Parse error: %s", parse_result$msg))
  }
  
  # Check 3: Non-ASCII characters (warning only)
  has_non_ascii <- any(grepl("[^\x01-\x7F]", raw_lines))
  if (has_non_ascii) {
    issues <- c(issues, "Warning: File contains non-ASCII characters")
  }
  
  return(list(
    filename          = spec$filename,
    exists            = TRUE,
    file_size         = file_size,
    record_count      = record_count,
    expected_rec_len  = spec$record_length,
    actual_rec_lens   = paste(unique(head(sort(unique(line_lengths)), 5)), collapse = ", "),
    n_cols            = spec$n_cols,
    issues            = if (length(issues) == 0) "PASS" else paste(issues, collapse = "; ")
  ))
}

#' Audit all downloaded files
#'
#' @param specs List of file specifications
#' @param output_dir Directory containing downloaded files
#' @return Data.table with audit results
audit_all_files <- function(specs, output_dir) {
  results <- lapply(specs, function(spec) {
    file_path <- file.path(output_dir, spec$filename)
    audit_file(spec, file_path)
  })
  
  # Convert to data.table
  results_dt <- rbindlist(lapply(results, as.data.table))
  
  return(results_dt)
}

#' Print formatted audit report
#'
#' @param audit_results Data.table from audit_all_files()
print_audit_report <- function(audit_results) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════════════════\n")
  cat("                    TCEQ PST DATA STRUCTURAL AUDIT REPORT                      \n")
  cat("═══════════════════════════════════════════════════════════════════════════════\n")
  cat("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Summary statistics
  n_files <- nrow(audit_results)
  n_passed <- sum(audit_results$issues == "PASS", na.rm = TRUE)
  n_failed <- n_files - n_passed
  
  cat("SUMMARY\n")
  cat("───────────────────────────────────────────────────────────────────────────────\n")
  cat(sprintf("  Total files checked:  %d\n", n_files))
  cat(sprintf("  Passed:               %d\n", n_passed))
  cat(sprintf("  Failed/Warnings:      %d\n", n_failed))
  cat("\n")
  
  # Detail for each file
  cat("FILE DETAILS\n")
  cat("───────────────────────────────────────────────────────────────────────────────\n")
  
  for (i in seq_len(nrow(audit_results))) {
    row <- audit_results[i]
    
    status_symbol <- if (row$issues == "PASS") "✓" else "⚠"
    
    cat(sprintf("\n%s %s\n", status_symbol, row$filename))
    
    if (row$exists) {
      cat(sprintf("    File size:       %s bytes\n", format(row$file_size, big.mark = ",")))
      cat(sprintf("    Record count:    %s\n", format(row$record_count, big.mark = ",")))
      cat(sprintf("    Expected width:  %d bytes\n", row$expected_rec_len))
      cat(sprintf("    Actual widths:   %s\n", row$actual_rec_lens))
      cat(sprintf("    Columns:         %d\n", row$n_cols))
      cat(sprintf("    Status:          %s\n", row$issues))
    } else {
      cat(sprintf("    Status:          %s\n", row$issues))
    }
  }
  
  cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
  cat("                              END OF AUDIT REPORT                              \n")
  cat("═══════════════════════════════════════════════════════════════════════════════\n\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4: Main Execution
# ══════════════════════════════════════════════════════════════════════════════

main <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════════════════\n")
  cat("                    00_download_texas.R - TCEQ PST Data Ingestion              \n")
  cat("═══════════════════════════════════════════════════════════════════════════════\n")
  cat("Output directory: ", OUTPUT_DIR, "\n")
  cat("Source: ", BASE_URL, "\n\n")
  
  # Step 1: Download all standard PST files
  cat("STEP 1: DOWNLOADING RAW FILES\n")
  cat("───────────────────────────────────────────────────────────────────────────────\n")
  
  download_results <- download_all_pst_files(FILE_SPECS, OUTPUT_DIR)
  
  # ───────────────────────────────────────────────────────────────────────────
  # Step 1.1: Historical Data & Readme (Placeholder)
  # ───────────────────────────────────────────────────────────────────────────
  # Placeholder code for historical construction data and readme docs.
  # Uncomment when ready to ingest.
  
  # historical_url <- "https://www.tceq.texas.gov/downloads/agency/data/lookup-data/12-3-construction-notification-extract-historical.xls"
  # readme_url     <- "https://www.tceq.texas.gov/downloads/agency/data/lookup-data/12-3-construction-notifications-readme.pdf"
  
  # message("⇣  Downloading Historical Data (Placeholder)...")
  # download.file(historical_url, file.path(OUTPUT_DIR, "12-3-construction-notification-extract-historical.xls"), mode = "wb", quiet = TRUE)
  # download.file(readme_url, file.path(OUTPUT_DIR, "12-3-construction-notifications-readme.pdf"), mode = "wb", quiet = TRUE)
  
  
  # Print download summary
  cat("\nDownload Summary:\n")
  cat(sprintf("  Successful: %d\n", sum(download_results$success)))
  cat(sprintf("  Failed:     %d\n", sum(!download_results$success)))
  
  if (any(!download_results$success)) {
    cat("\nFailed downloads:\n")
    failed <- download_results[success == FALSE]
    for (i in seq_len(nrow(failed))) {
      cat(sprintf("  - %s: %s\n", failed$filename[i], failed$error[i]))
    }
  }
  
  # Step 2: Audit downloaded files
  cat("\n")
  cat("STEP 2: STRUCTURAL AUDIT\n")
  cat("───────────────────────────────────────────────────────────────────────────────\n")
  
  audit_results <- audit_all_files(FILE_SPECS, OUTPUT_DIR)
  
  # Print audit report
  print_audit_report(audit_results)
  
  # Save audit results
  audit_path <- file.path(OUTPUT_DIR, "00_audit_results.csv")
  fwrite(audit_results, audit_path)
  message("Audit results saved to: ", audit_path)
  
  # Return combined results
  invisible(list(
    download_results = download_results,
    audit_results    = audit_results
  ))
}

# ══════════════════════════════════════════════════════════════════════════════
# RUN
# ══════════════════════════════════════════════════════════════════════════════

if (!interactive()) {
  main()
} else {
  message("Script loaded. Run main() to execute.")
}