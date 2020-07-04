*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.07.2019 at 13:53:01 by user XAERTURK
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSOG_FI_002_C001................................*
DATA:  BEGIN OF STATUS_ZSOG_FI_002_C001              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSOG_FI_002_C001              .
CONTROLS: TCTRL_ZSOG_FI_002_C001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSOG_FI_002_C001              .
TABLES: ZSOG_FI_002_C001               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
