*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_004_SALES_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsog_fi_010_sales_upload.

INCLUDE zsog_fi_010_sales_upload_top.
INCLUDE zsog_fi_010_sales_upload_scr.
INCLUDE zsog_fi_010_sales_upload_f01.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM f4_filename CHANGING p_file.

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
