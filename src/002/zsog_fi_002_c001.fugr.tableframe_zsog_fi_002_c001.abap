*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSOG_FI_002_C001
*   generation date: 18.07.2019 at 13:52:59 by user XAERTURK
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSOG_FI_002_C001   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
