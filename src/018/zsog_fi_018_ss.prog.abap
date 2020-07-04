*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_018_SS
*&---------------------------------------------------------------------*
TABLES: bsik, t012k, regup, lfa1, ZSOG_FI_018_T_01.
TYPE-POOLS: icon, sscr.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .

SELECT-OPTIONS:  s_bukrs  FOR t012k-bukrs NO INTERVALS NO-EXTENSION MODIF ID m2,
                 s_waers  FOR bsik-waers  NO INTERVALS NO-EXTENSION MODIF ID m1,
                 s_belnr  FOR bsik-belnr  MODIF ID c1,
                 s_lifnr  FOR bsik-lifnr  MODIF ID c1,
                 s_tarih  FOR bsik-budat  NO-EXTENSION MODIF ID m1,
                 s_group  FOR lfa1-ktokk  MODIF ID c1.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS:  s_laufd  FOR regup-laufd  NO INTERVALS NO-EXTENSION DEFAULT sy-datum MODIF ID m2,
                 s_laufi  FOR ZSOG_FI_018_T_01-laufi NO INTERVALS NO-EXTENSION DEFAULT 'Z001' MODIF ID m2 .
SELECTION-SCREEN END OF BLOCK b5.


SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-038.
PARAMETERS:      p_oneri  TYPE bsik-wrbtr MODIF ID c1.
SELECTION-SCREEN END OF BLOCK b6.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004 .
SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN COMMENT 1(29) TEXT-005 FOR FIELD p_hbkid MODIF ID c1.
PARAMETERS: p_hbkid  TYPE t012k-hbkid MODIF ID m1.
SELECTION-SCREEN COMMENT 44(40) TEXT-006 MODIF ID c1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-007.
PARAMETERS : p_r1 RADIOBUTTON GROUP g1 USER-COMMAND cmd DEFAULT 'X' MODIF ID c2,
             p_r2 RADIOBUTTON GROUP g1 MODIF ID c2.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-008.
PARAMETERS : p_mode TYPE apqi-putactive DEFAULT 'N' MODIF ID m1 NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b1.