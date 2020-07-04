*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_004_SALES_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSOG_FI_004_SALES_UPLOAD.

INCLUDE: ZSOG_FI_004_SALES_UPLOAD_top,
         ZSOG_FI_004_SALES_UPLOAD_scr,
         ZSOG_FI_004_SALES_UPLOAD_f01.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_filename CHANGING p_file.

INITIALIZATION.
  PERFORM initialize_program.

AT SELECTION-SCREEN.
  PERFORM at_sscr.

INITIALIZATION.
  PERFORM initialization.


START-OF-SELECTION.
  PERFORM processing.
  PERFORM initialize_alv.
  PERFORM display_alv.
