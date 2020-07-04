*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_001_CUST_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsog_fi_001_cust_create.

INCLUDE: zsog_fi_001_cust_create_top,
         zsog_fi_001_cust_create_scr,
         zsog_fi_001_cust_create_f01.

START-OF-SELECTION.
  PERFORM processing.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_filename CHANGING p_file.

INITIALIZATION.
  PERFORM initialize_program.

AT SELECTION-SCREEN.
  PERFORM at_sscr.
