*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 07.08.2019 at 11:10:57 by user XTBIRIHAN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSOG_FI_008_T_02................................*
DATA:  BEGIN OF STATUS_ZSOG_FI_008_T_02              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSOG_FI_008_T_02              .
CONTROLS: TCTRL_ZSOG_FI_008_T_02
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSOG_FI_008_T_02              .
TABLES: ZSOG_FI_008_T_02               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
