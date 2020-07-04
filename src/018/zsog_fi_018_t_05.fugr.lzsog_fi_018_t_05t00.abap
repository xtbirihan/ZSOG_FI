*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.03.2020 at 12:24:24 by user XOSAHIN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSOG_FI_018_T_05................................*
DATA:  BEGIN OF STATUS_ZSOG_FI_018_T_05              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSOG_FI_018_T_05              .
CONTROLS: TCTRL_ZSOG_FI_018_T_05
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSOG_FI_018_T_05              .
TABLES: ZSOG_FI_018_T_05               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
