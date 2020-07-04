*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 22.08.2019 at 11:33:56 by user XTBIRIHAN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSOG_FI_004_T_04................................*
DATA:  BEGIN OF STATUS_ZSOG_FI_004_T_04              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSOG_FI_004_T_04              .
CONTROLS: TCTRL_ZSOG_FI_004_T_04
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSOG_FI_004_T_04              .
TABLES: ZSOG_FI_004_T_04               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
