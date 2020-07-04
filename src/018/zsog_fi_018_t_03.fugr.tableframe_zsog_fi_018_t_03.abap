*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSOG_FI_018_T_03
*   generation date: 11.03.2020 at 17:05:15 by user XOSAHIN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSOG_FI_018_T_03   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
