*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSOG_FI_010
*   generation date: 17.10.2019 at 14:57:44 by user XEBUDAK
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSOG_FI_010        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
