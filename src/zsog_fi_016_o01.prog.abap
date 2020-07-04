*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_016_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: itab TYPE TABLE OF sy-ucomm.

  CLEAR: itab, itab[].

  IF r1 = 'X'.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF r2 = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF ra = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF rb = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF r3 = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF r4 = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF r5 = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF r6 = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF rc = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
*    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF rd = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
*    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF r7 = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
*    APPEND 'SB_HAFTA'   TO itab.
    APPEND 'SB_HAFTA_R' TO itab.
  ELSEIF r8 = 'X'.
    APPEND 'BB_GUNLUK'  TO itab.
    APPEND 'BB_GUN_REV' TO itab.
    APPEND 'BB_TH'      TO itab.
    APPEND 'BB_TH_R'    TO itab.
    APPEND 'BB_HAFTA'   TO itab.
    APPEND 'BB_HAFTA_R' TO itab.
    APPEND 'SB_GUNLUK'  TO itab.
    APPEND 'SB_GUN_REV' TO itab.
    APPEND 'SB_TH'      TO itab.
    APPEND 'SB_TH_R'    TO itab.
    APPEND 'SB_HAFTA'   TO itab.
*    APPEND 'SB_HAFTA_R' TO itab.
  ENDIF.


  SET PF-STATUS 'STATUS1' EXCLUDING itab.
  SET TITLEBAR  'TTITLE1'.

  PERFORM show_alv.

ENDMODULE.                 " STATUS_0100  OUTPUT
