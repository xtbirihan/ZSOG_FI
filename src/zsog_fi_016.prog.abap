*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_016
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsog_fi_016.

INCLUDE zsog_fi_016_top.
INCLUDE zsog_fi_016_f01.
INCLUDE zsog_fi_016_o01.
INCLUDE zsog_fi_016_i01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_accper-low.

  PERFORM init_accper_week USING 'S_ACCPER-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_week-low.

  PERFORM init_accper_week USING 'S_WEEK-LOW'.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

START-OF-SELECTION.
  PERFORM check_data CHANGING gv_error.

  CHECK gv_error = space.

  PERFORM get_data CHANGING gv_error.

  CHECK gv_error = space.

  PERFORM show_data.
