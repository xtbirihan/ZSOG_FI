*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_011_SALES_COLLECT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSOG_FI_011_SALES_COLLECT.

INCLUDE: ZSOG_FI_011_SALES_COLLECT_scr,
         ZSOG_FI_011_SALES_COLLECT_f01.

START-OF-SELECTION.

PERFORM collect_data.
