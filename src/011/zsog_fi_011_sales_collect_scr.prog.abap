*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_011_SALES_COLLECT_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
SELECT-OPTIONS: s_tarih FOR sy-datum NO-EXTENSION OBLIGATORY .
*parameters: p_file like rlgrap-filename NO-DISPLAY  DEFAULT 'C:/' .
SELECTION-SCREEN END OF BLOCK b1.
