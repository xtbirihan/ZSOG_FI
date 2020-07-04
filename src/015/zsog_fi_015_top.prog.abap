*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_015_TOP
*&---------------------------------------------------------------------*

TABLES: t001, bsid , bsad , bsik, bsak, skat, zsog_fi_015_s_01, kna1 ,
  tcurr.

*DATA:      gv_trh1 type char10 ,
*           gv_trh2 type char10 ,
*           gv_trh3 type char10 ,
*           gv_trh4 type char10 .
*
*CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4)
*into gv_trh1.


SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
PARAMETERS : rb_gun RADIOBUTTON GROUP rd1 USER-COMMAND rb DEFAULT 'X',
             rb_trh RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE text-001.
PARAMETERS: p_bukrs LIKE t001-bukrs OBLIGATORY,
            p_keydt LIKE bsik-budat OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN SKIP 1.

PARAMETERS : rb_mus RADIOBUTTON GROUP radi USER-COMMAND ms DEFAULT 'X',
             rb_sat RADIOBUTTON GROUP radi,
             p_check AS CHECKBOX,
             p_is AS CHECKBOX.

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_kunnr FOR bsid-kunnr,
                s_lifnr FOR bsik-lifnr,
                s_hkont FOR bsik-hkont,
                s_regio FOR kna1-regio.
PARAMETERS :    s_gsber LIKE bsid-gsber.

SELECTION-SCREEN END OF BLOCK bl01.
PARAMETERS: land1 TYPE kna1-land1 DEFAULT 'TR' NO-DISPLAY.
SELECTION-SCREEN BEGIN OF BLOCK bl02 WITH FRAME TITLE text-002.
PARAMETERS : rb_try  RADIOBUTTON GROUP rcur DEFAULT 'X',
*             rb_eur  RADIOBUTTON GROUP rcur,
             rb_bpb  RADIOBUTTON GROUP rcur.

SELECTION-SCREEN BEGIN OF BLOCK bl02_2 WITH FRAME.
PARAMETERS : rb_date  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl02_2.

SELECTION-SCREEN END OF BLOCK bl02.
*SELECTION-SCREEN BEGIN OF BLOCK bl04 WITH FRAME TITLE text-004.
*PARAMETERS : rb_ozet   RADIOBUTTON GROUP rad DEFAULT 'X',
*             rb_detay  RADIOBUTTON GROUP rad.
*SELECTION-SCREEN END OF BLOCK bl04.
SELECTION-SCREEN BEGIN OF BLOCK bl03 WITH FRAME TITLE text-003.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) text-gcm FOR FIELD p_gcm1 MODIF ID gun.
PARAMETERS: p_gcm1(2)  TYPE n  OBLIGATORY DEFAULT '07'.

SELECTION-SCREEN COMMENT 35(1) text-arl FOR FIELD p_gcm2 MODIF ID gun.
PARAMETERS: p_gcm2(2)  TYPE n  OBLIGATORY DEFAULT '30'.

SELECTION-SCREEN COMMENT 40(1) text-arl FOR FIELD p_gcm3 MODIF ID gun.
PARAMETERS: p_gcm3(3)  TYPE n  OBLIGATORY DEFAULT '60'.

SELECTION-SCREEN COMMENT 46(1) text-arl FOR FIELD p_gcm4 MODIF ID gun.
PARAMETERS: p_gcm4(3)  TYPE n  OBLIGATORY DEFAULT '90'.

SELECTION-SCREEN COMMENT 52(1) text-arl FOR FIELD p_gcm5 MODIF ID gun.
PARAMETERS: p_gcm5(3)  TYPE n  OBLIGATORY DEFAULT '120'.

SELECTION-SCREEN COMMENT 58(1) text-arl FOR FIELD p_gcm6 MODIF ID gun.
PARAMETERS: p_gcm6(3)  TYPE n  OBLIGATORY DEFAULT '150'.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:11:28
SELECTION-SCREEN COMMENT 64(1) text-arl FOR FIELD p_gcm7 MODIF ID gun.
PARAMETERS: p_gcm7(3)  TYPE n  OBLIGATORY DEFAULT '180'.

SELECTION-SCREEN COMMENT 70(1) text-arl FOR FIELD p_gcm8 MODIF ID gun.
PARAMETERS: p_gcm8(3)  TYPE n  OBLIGATORY DEFAULT '210'.
*}    <<<- End of  Added - 07.10.2019 10:11:28

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) text-ilr FOR FIELD p_ilr1 MODIF ID gun.
PARAMETERS: p_ilr1(2)  TYPE n  OBLIGATORY DEFAULT '07'.

SELECTION-SCREEN COMMENT 35(1) text-arl FOR FIELD p_ilr2 MODIF ID gun.
PARAMETERS: p_ilr2(2)  TYPE n  OBLIGATORY DEFAULT '30'.

SELECTION-SCREEN COMMENT 40(1) text-arl FOR FIELD p_ilr3 MODIF ID gun.
PARAMETERS: p_ilr3(3)  TYPE n  OBLIGATORY DEFAULT '60'.

SELECTION-SCREEN COMMENT 46(1) text-arl FOR FIELD p_ilr4 MODIF ID gun.
PARAMETERS: p_ilr4(3)  TYPE n  OBLIGATORY DEFAULT '90'.

SELECTION-SCREEN COMMENT 52(1) text-arl FOR FIELD p_ilr3 MODIF ID gun.
PARAMETERS: p_ilr5(3)  TYPE n  OBLIGATORY DEFAULT '120'.

SELECTION-SCREEN COMMENT 58(1) text-arl FOR FIELD p_ilr4 MODIF ID gun.
PARAMETERS: p_ilr6(3)  TYPE n  OBLIGATORY DEFAULT '150'.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:11:52
SELECTION-SCREEN COMMENT 64(1) text-arl FOR FIELD p_ilr3 MODIF ID gun.
PARAMETERS: p_ilr7(3)  TYPE n  OBLIGATORY DEFAULT '180'.

SELECTION-SCREEN COMMENT 70(1) text-arl FOR FIELD p_ilr4 MODIF ID gun.
PARAMETERS: p_ilr8(3)  TYPE n  OBLIGATORY DEFAULT '210'.
*}    <<<- End of  Added - 07.10.2019 10:11:52

SELECTION-SCREEN END OF LINE.

***********************************************************************
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(30) text-gcm FOR FIELD p_gcm5 MODIF ID trh.
*PARAMETERS: p_gcm5(2)  TYPE n  OBLIGATORY DEFAULT '07'.
*
*SELECTION-SCREEN COMMENT 35(1) text-arl FOR FIELD p_gcm6 MODIF ID trh.
*PARAMETERS: p_gcm6(2)  TYPE n  OBLIGATORY DEFAULT '30'.
*
*SELECTION-SCREEN COMMENT 40(1) text-arl FOR FIELD p_gcm7 MODIF ID trh.
*PARAMETERS: p_gcm7(3)  TYPE n  OBLIGATORY DEFAULT '60'.
*
*SELECTION-SCREEN COMMENT 46(1) text-arl FOR FIELD p_gcm8 MODIF ID trh.
*PARAMETERS: p_gcm8(3)  TYPE n  OBLIGATORY DEFAULT '90'.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(30) text-ilr FOR FIELD p_ilr5 MODIF ID trh.
*PARAMETERS: p_ilr5(2)  TYPE n  OBLIGATORY DEFAULT '07'.
*
*SELECTION-SCREEN COMMENT 35(1) text-arl FOR FIELD p_ilr6 MODIF ID trh.
*PARAMETERS: p_ilr6(2)  TYPE n  OBLIGATORY DEFAULT '30'.
*
*SELECTION-SCREEN COMMENT 40(1) text-arl FOR FIELD p_ilr7 MODIF ID trh.
*PARAMETERS: p_ilr7(3)  TYPE n  OBLIGATORY DEFAULT '60'.
*
*SELECTION-SCREEN COMMENT 46(1) text-arl FOR FIELD p_ilr8 MODIF ID trh.
*PARAMETERS: p_ilr8(3)  TYPE n  OBLIGATORY DEFAULT '90'.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl03.
DATA:     ld_pri_params  LIKE pri_params,
        ld_valid,
        gt_lfa1 TYPE lfa1 OCCURS 0 WITH HEADER LINE,
        gt_lfa2 TYPE lfa1 OCCURS 0 WITH HEADER LINE,
        gt_kna1 TYPE kna1 OCCURS 0 WITH HEADER LINE,
        gt_kna2 TYPE kna1 OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF tb_knb1 OCCURS 0,
         kunnr  LIKE knb1-kunnr,
         akont  LIKE knb1-akont,
         name1  LIKE kna1-name1,
       END   OF tb_knb1,
       BEGIN OF tb_knb2 OCCURS 0,
         kunnr  LIKE knb1-kunnr,
         akont  LIKE knb1-akont,
         name1  LIKE kna1-name1,
       END   OF tb_knb2,

       BEGIN OF tb_bsid  OCCURS 0,
         kunnr         LIKE bsid-kunnr,
         belnr         LIKE bsid-belnr,
         xblnr         LIKE bsid-xblnr,
         budat         LIKE bsid-budat,
         umskz         LIKE bsid-umskz,
         shkzg         LIKE bsid-shkzg,
         zfbdt         LIKE bsid-zfbdt,
         zbd1t         LIKE bsid-zbd1t,
         dmbtr         LIKE bsid-dmbtr,
         dmbe2         LIKE bsid-dmbe2,
         filkd         LIKE bsid-filkd,
         gsber         LIKE bsid-gsber,
         waers         LIKE bsid-waers,
         wrbtr         LIKE bsid-wrbtr,
       END   OF tb_bsid,

       tb_bsad  LIKE tb_bsid OCCURS 1 WITH HEADER LINE ,
       tb_bsad2 LIKE tb_bsid OCCURS 1 WITH HEADER LINE ,
       tb_bsid2 LIKE tb_bsid OCCURS 1 WITH HEADER LINE ,

       BEGIN OF tb_borc OCCURS 1,
        alan       LIKE bsid-kunnr,
        kunnr      LIKE bsid-kunnr,
        belnr      LIKE bsid-belnr,
        xblnr      LIKE bsid-xblnr,
        budat      LIKE bsid-budat,
        zfbdt      LIKE bsid-zfbdt,
        dmbtr      LIKE bsid-dmbtr,
        dmbe2      LIKE bsid-dmbe2,
        kalan      LIKE bsid-dmbtr,
        carpim     TYPE bapicurr_d,
        gun(16)    TYPE p DECIMALS 2 ,
        gsber      LIKE bsid-gsber,
        waers      LIKE bsid-waers,
        wrbtr         LIKE bsid-wrbtr,
       END   OF tb_borc,

       tb_alac  LIKE tb_borc OCCURS 0 WITH HEADER LINE,
       tb_alac2  LIKE tb_borc OCCURS 0 WITH HEADER LINE,
       tb_borc2  LIKE tb_borc OCCURS 0 WITH HEADER LINE,
       tb_borc3  LIKE tb_borc OCCURS 0 WITH HEADER LINE,
       tb_ozet  LIKE tb_borc OCCURS 0 WITH HEADER LINE,
       tb_list  LIKE zsog_fi_015_s_01 OCCURS 0 WITH HEADER LINE,
       tb_msy   LIKE tb_list OCCURS 0 WITH HEADER LINE,
       tb_listd LIKE tb_list OCCURS 0 WITH HEADER LINE,
       va_repid LIKE sy-repid.

DATA:BEGIN OF it_tdetay OCCURS 0,
  kunnr  LIKE bsid-kunnr,
  belnr  LIKE bsid-belnr,
  txt50  LIKE skat-txt50,
  xblnr  LIKE bsid-xblnr,
  budat  LIKE bsid-budat,
  zfbdt  LIKE bsid-zfbdt,
  name1  LIKE zsog_fi_015_s_01-name1,
  bakiy  LIKE bsid-dmbtr,
  ortgn  LIKE zsog_fi_015_s_01-ortgn,
  gvb99  LIKE bsid-dmbtr,
  gvb08  LIKE bsid-dmbtr,
  gvb07  LIKE bsid-dmbtr,
  gvb06  LIKE bsid-dmbtr,
  gvb05  LIKE bsid-dmbtr,
  gvb04  LIKE bsid-dmbtr,
  gvb03  LIKE bsid-dmbtr,
  gvb02  LIKE bsid-dmbtr,
  gvb01  LIKE bsid-dmbtr,
  gvbtop LIKE bsid-dmbtr,
  gvbgn  LIKE zsog_fi_015_s_01-gvbgn,
  ivb01  LIKE bsid-dmbtr,
  ivb02  LIKE bsid-dmbtr,
  ivb03  LIKE bsid-dmbtr,
  ivb04  LIKE bsid-dmbtr,
  ivb05  LIKE bsid-dmbtr,
  ivb06  LIKE bsid-dmbtr,
  ivb07  LIKE bsid-dmbtr,
  ivb08  LIKE bsid-dmbtr,
  ivb99  LIKE bsid-dmbtr,
  ivbtop LIKE bsid-dmbtr,
  ivbgn  LIKE zsog_fi_015_s_01-ivbgn,
  waers  LIKE bsid-waers,
  END OF it_tdetay.

RANGES : r_gvb99 FOR sy-datum,
         r_gvb01 FOR sy-datum,
         r_gvb02 FOR sy-datum,
         r_gvb03 FOR sy-datum,
         r_gvb04 FOR sy-datum,
         r_gvb05 FOR sy-datum,
         r_gvb06 FOR sy-datum,

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:41:18
         r_gvb07 FOR sy-datum,
         r_gvb08 FOR sy-datum,
*         }  	<<<- End of  Added - 07.10.2019 10:41:18

         r_ivb01 FOR sy-datum,
         r_ivb02 FOR sy-datum,
         r_ivb03 FOR sy-datum,
         r_ivb04 FOR sy-datum,
         r_ivb05 FOR sy-datum,
         r_ivb06 FOR sy-datum,

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:41:29
         r_ivb07 FOR sy-datum,
         r_ivb08 FOR sy-datum,
*         }  	<<<- End of  Added - 07.10.2019 10:41:29

         r_ivb99 FOR sy-datum.
*&---alv definitions-------------------------------------------------*
TYPE-POOLS: slis.
DATA : lt_header TYPE slis_listheader OCCURS 0 WITH HEADER LINE.
DATA : ls_layout TYPE slis_layout_alv,
       lt_fcat   TYPE slis_t_fieldcat_alv,
       gs_fcat   LIKE LINE OF lt_fcat, "added by xstaskent 03.10.2019
       l_disvariant LIKE disvariant,
       lt_sort   TYPE slis_sortinfo_alv OCCURS 0 WITH HEADER LINE.
DATA : gt_events TYPE slis_t_event.

*DATA: gs_liste_yetki TYPE zmuh_liste_yetki.
DATA: gv_sistem_yon(1).

DATA:gv_hata.

DATA seats_tab LIKE SORTED TABLE OF tb_list
WITH NON-UNIQUE KEY  alan hkont txt50 kunnr
name1 waers gsber.

DATA seats_tab_2 LIKE SORTED TABLE OF tb_list
WITH NON-UNIQUE KEY  alan hkont txt50 kunnr
name1 waers gsber.

DATA seats_tab_3 LIKE SORTED TABLE OF tb_list
WITH UNIQUE KEY  alan hkont txt50 kunnr
name1 gsber waers.
