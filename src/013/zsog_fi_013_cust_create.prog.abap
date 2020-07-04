*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_006_CUST_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsog_fi_013_cust_create.

INCLUDE zsog_fi_013_cust_create_top.
INCLUDE zsog_fi_013_cust_create_scr.
INCLUDE zsog_fi_013_cust_create_f01.

INITIALIZATION.

START-OF-SELECTION.
  PERFORM create_tables.
  IF sy-batch NE 'X'.
    PERFORM initialize_alv.
    PERFORM display_alv.
  ELSE.
    PERFORM create_customer.
    PERFORM initialize_alv.
    PERFORM display_alv.
  ENDIF.


INITIALIZATION.
  PERFORM initialize_program.

AT SELECTION-SCREEN.
  PERFORM download_sample_csv.
