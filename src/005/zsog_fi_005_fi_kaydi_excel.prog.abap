*&---------------------------------------------------------------------*
*  ABAP Name     : ZSOG_FI_005_FI_KAYDI_EXCEL
*  Job-Name      :
*  Author        : Burcu Hilal Altunbaş
*  e-mail        : burcu.altunbas@prodea.com.tr
*  GMP relevant  : Mert Kaya
*  Date          : 27.07.2019
*  Description   : Excel ile FI Kaydı Yaratma Programı
*&---------------------------------------------------------------------*

REPORT zsog_fi_005_fi_kaydi_excel.

INCLUDE zsog_sd_object_alv.
INCLUDE zsog_fi_document.
INCLUDE zsog_fi_forms.
INCLUDE zsog_fi_excel_top.
INCLUDE zsog_fi_excel_f01.
INCLUDE zsog_fi_excel_i01.
INCLUDE zsog_fi_excel_o01.

*--------------------------- START PROGRAM ----------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.

AT SELECTION-SCREEN.
  PERFORM excel_control.
  IF sy-ucomm = 'CL1'.
    PERFORM excel_download_sample.
  ENDIF.

INITIALIZATION.

START-OF-SELECTION.

*- Init program
  PERFORM init_program.
*- İnternal table doldur
  PERFORM fill_internal_table.
*- ALV doldur
  PERFORM write_alv.
*- Display Screen
  PERFORM display_screen.

END-OF-SELECTION.
