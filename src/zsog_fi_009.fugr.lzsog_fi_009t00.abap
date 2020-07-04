*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 15.10.2019 at 10:45:49 by user XEBUDAK
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSOG_FI_009.....................................*
DATA:  BEGIN OF STATUS_ZSOG_FI_009                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSOG_FI_009                   .
CONTROLS: TCTRL_ZSOG_FI_009
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSOG_FI_009                   .
TABLES: ZSOG_FI_009                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
