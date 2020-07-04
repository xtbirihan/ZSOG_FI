*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_004_SALES_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSOG_FI_012_SALES_ENTG.

INCLUDE ZSOG_FI_012_SALES_ENTG_TOP.
INCLUDE ZSOG_FI_012_SALES_ENTG_SCR.
INCLUDE ZSOG_FI_012_SALES_ENTG_F01.


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
