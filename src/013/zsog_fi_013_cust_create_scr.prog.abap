*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_006_CUST_CREATE_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_date FOR sy-datum OBLIGATORY NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.
*SELECTION-SCREEN FUNCTION KEY 1.
*SELECTION-SCREEN FUNCTION KEY 2.
