*&---------------------------------------------------------------------*
*  ABAP Name     : ZSOG_FI_002_BAYI_VADE
*  Job-Name      :
*  Author        : Aras Ertürk
*  e-mail        : aras.erturk@prodea.com.tr
*  Fn. consultant: Mustafa Şahin
*  Date          : 18.07.2019 14:17:36
*  Transport     : DHDK946900
*  Description   : Mobil bayiler için vade hesaplama programı
*----------------------------------------------------------------------*
*  Changes                                                             *
*----------------------------------------------------------------------*
*  Date      Name      No.   Description                               *
*----------------------------------------------------------------------*

REPORT zsog_fi_002_bayi_vade.

INCLUDE zsog_fi_002_bayi_vade_top.
INCLUDE zsog_fi_002_bayi_vade_f01.
INCLUDE zsog_fi_002_bayi_vade_o01.
INCLUDE zsog_fi_002_bayi_vade_i01.

START-OF-SELECTION.
  PERFORM check_data.
  CHECK gv_error IS INITIAL.
  PERFORM get_data.
