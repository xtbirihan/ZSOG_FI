*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.03.2020 at 17:05:16 by user XOSAHIN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSOG_FI_018_T_03................................*
DATA:  BEGIN OF STATUS_ZSOG_FI_018_T_03              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSOG_FI_018_T_03              .
CONTROLS: TCTRL_ZSOG_FI_018_T_03
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZSOG_FI_018_T_03              .
TABLES: ZSOG_FI_018_T_03               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
