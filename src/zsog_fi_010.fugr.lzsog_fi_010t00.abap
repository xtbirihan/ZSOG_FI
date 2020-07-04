*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 17.10.2019 at 14:57:46 by user XEBUDAK
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSOG_FI_010.....................................*
DATA:  BEGIN OF STATUS_ZSOG_FI_010                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSOG_FI_010                   .
CONTROLS: TCTRL_ZSOG_FI_010
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSOG_FI_010                   .
TABLES: ZSOG_FI_010                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
