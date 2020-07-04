*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSOG_FI_018_T_05
*   generation date: 18.03.2020 at 12:24:21 by user XOSAHIN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSOG_FI_018_T_05   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
