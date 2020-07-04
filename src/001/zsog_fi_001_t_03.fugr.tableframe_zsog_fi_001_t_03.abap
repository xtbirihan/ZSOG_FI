*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSOG_FI_001_T_03
*   generation date: 20.08.2019 at 18:28:04 by user XDPOLAT
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSOG_FI_001_T_03   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
