*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 20.08.2019 at 18:28:06 by user XDPOLAT
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSOG_FI_001_T_03................................*
DATA:  BEGIN OF STATUS_ZSOG_FI_001_T_03              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSOG_FI_001_T_03              .
CONTROLS: TCTRL_ZSOG_FI_001_T_03
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZSOG_FI_001_T_03              .
TABLES: ZSOG_FI_001_T_03               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
