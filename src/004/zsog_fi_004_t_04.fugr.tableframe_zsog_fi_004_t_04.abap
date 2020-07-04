*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSOG_FI_004_T_04
*   generation date: 22.08.2019 at 11:33:55 by user XTBIRIHAN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSOG_FI_004_T_04   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
