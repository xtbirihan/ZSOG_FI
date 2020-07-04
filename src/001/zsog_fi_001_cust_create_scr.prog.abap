*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_001_CUST_CREATE_SCR
*&---------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-000.
parameters: p_file like rlgrap-filename obligatory DEFAULT 'C:/'.
selection-screen end of block b1.
selection-screen function key 1.
