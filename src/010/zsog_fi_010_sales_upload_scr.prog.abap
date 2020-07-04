*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_004_SALES_UPLOAD_SCR
*&---------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-000.
SELECT-OPTIONS: s_tarih FOR sy-datum no-EXTENSION OBLIGATORY .
*parameters: p_file like rlgrap-filename NO-DISPLAY  DEFAULT 'C:/' .
selection-screen end of block b1.
*selection-screen function key 1.
