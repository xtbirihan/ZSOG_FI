*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_006_CUST_CREATE_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_rmf LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:/',
            p_rlf LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:/'.
SELECTION-SCREEN END OF BLOCK b1.
*SELECTION-SCREEN FUNCTION KEY 1.
*SELECTION-SCREEN FUNCTION KEY 2.
