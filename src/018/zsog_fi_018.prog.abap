*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_018
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsog_fi_018 MESSAGE-ID zsog_fi_018.

INCLUDE: zsog_fi_018_ss , "selection screen
         zsog_fi_018_top, "data declaration
         zsog_fi_018_evn, "alv events
         zsog_fi_018_f01, "form routins
         zsog_fi_018_f02, "grid alv form routins
         zsog_fi_018_o01, "pbo
         zsog_fi_018_i01. "pai

AT SELECTION-SCREEN.
  PERFORM control_fields.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_laufi-low.
  PERFORM search_help_for_laufi.


INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.

  PERFORM authority_check.

  IF p_r1 IS NOT INITIAL.
*    PERFORM lock_object."xosahin
    PERFORM header_data.
    PERFORM get_data.
    PERFORM check_data.
*    PERFORM initialize_alv TABLES gs_scr_1903-sum_alv USING 'STATUS'."bug olduğu için kopyalandı
    PERFORM initialize_alv TABLES gs_scr_1903-sum_alv USING 'STANDART3'."ozans
    PERFORM display_alv.
  ELSEIF p_r2 IS NOT INITIAL.
*    PERFORM lock_object."xosahin
    PERFORM header_data.
    PERFORM get_list_report.
    PERFORM initialize_alv TABLES gs_scr_1903-sum_alv USING 'LISTERPRT'.
    PERFORM display_alv.
  ENDIF.
