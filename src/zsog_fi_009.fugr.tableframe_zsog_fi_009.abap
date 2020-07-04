*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSOG_FI_009
*   generation date: 15.10.2019 at 10:45:49 by user XEBUDAK
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSOG_FI_009        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
