*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_017
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSOG_FI_017.

INCLUDE: ZSOG_FI_017_TOP,
         ZSOG_FI_017_C01,
         ZSOG_FI_017_F01,
         ZSOG_FI_017_001,
         ZSOG_FI_017_I01.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_open_file.

  AT SELECTION-SCREEN.
  PERFORM excel_control.
  IF sy-ucomm = 'CL1'.
    PERFORM excel_download_sample.
  ENDIF.

START-OF-SELECTION.
  PERFORM excel_upload.
  CHECK gv_error IS INITIAL.
  PERFORM controls.
END-OF-SELECTION.
  CALL SCREEN 0100.
