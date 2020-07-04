*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_019_OTOMATIK_ODEME
*&
*&---------------------------------------------------------------------*
*  ABAP Name     : ZSOG_FI_019_OTOMATIK_ODEME
*  Job-Name      :
*  Author        : ozan şahin
*  e-mail        :
*  GMP relevant  : Cemre Baker
*  Date          : 20.03.2020 15:31:24
*  Transport     :
*  Açıklama      : Otomatik ödeme programı
*----------------------------------------------------------------------*
*  Changes                                                             *
*----------------------------------------------------------------------*
*  Date      Name      No.   Description                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zsog_fi_019_otomatik_odeme.

INCLUDE zsog_fi_019_otomatik_odeme_top.
INCLUDE zsog_fi_019_otomatik_odeme_sub.
INCLUDE zsog_fi_019_otomatik_odeme_mod.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_laufi.
  PERFORM f_get_laufi.

START-OF-SELECTION .
  PERFORM get_data .
  PERFORM modify_data.
  PERFORM show_data .
