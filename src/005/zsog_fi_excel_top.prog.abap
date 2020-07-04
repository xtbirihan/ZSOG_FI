*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_EXCEL_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS: truxs, icon.

TYPES : BEGIN OF  t_datatab ,
        belge_no(10) , " Ynternal verilecek belge no
        budat(10), " Kayyt Tarihi
        bldat(10), " Belge Tarihi
        blart LIKE bkpf-blart , " Belge türü
        bukrs LIKE bkpf-bukrs , " ?irket Kodu
        waers(3) , " Para birimi
        kursf(16) ,
        xblnr LIKE bkpf-xblnr , " Referans No
        bktxt LIKE bkpf-bktxt,
        shkzg(1),  "kayıt anahtarı
        koart(1),
        hkont(10) ,
        umskz(1),
        wrbtr(16) ,
        dmbtr(16) ,
        mwskz(2),
        zfbdt(10),
        zterm(4),
        valut(10),
        kostl(10),
        prctr(10),
        aufnr(12),
        projk(24),
        gsber(4),
        zuonr(18),
        sgtxt(50),
        xref1(12),
        xref2(12),
        xref3(12),
*-------------------------------------------------------
        ldgrp LIKE bkpf-ldgrp ,
*-------------------------------------------------------
        END OF t_datatab .
DATA : it_datatab TYPE STANDARD TABLE OF t_datatab,
       wa_datatab TYPE t_datatab.

DATA : it_raw TYPE truxs_t_text_data.

DATA : BEGIN OF gt_data OCCURS 0 ,
        belge_no(10) , " Ynternal verilecek belge no
        budat LIKE bkpf-budat,
        bldat LIKE bkpf-bldat,
        blart LIKE bkpf-blart , " Belge türü
        bukrs LIKE bkpf-bukrs , " ?irket Kodu
        waers LIKE bkpf-waers ,
        kursf LIKE bkpf-kursf ,
        xblnr LIKE bkpf-xblnr , " Referans No
        bktxt LIKE bkpf-bktxt,
        shkzg LIKE bseg-shkzg,
        koart LIKE bseg-koart,
        hkont LIKE bseg-hkont,
        umskz LIKE bseg-umskz,
        wrbtr LIKE bseg-wrbtr,
        dmbtr LIKE bseg-dmbtr,
        mwskz LIKE bseg-mwskz,
        zfbdt LIKE bseg-zfbdt,
        zterm LIKE bseg-zterm,
        valut LIKE bseg-valut,
        kostl LIKE bseg-kostl,
        prctr LIKE bseg-prctr,
        aufnr LIKE bseg-aufnr,
        projk LIKE bseg-projk,
*        projk(24),
        gsber LIKE bseg-gsber,
        zuonr LIKE bseg-zuonr,
        sgtxt LIKE bseg-sgtxt,
        xref1 LIKE bseg-xref1,
        xref2 LIKE bseg-xref2,
        xref3 LIKE bseg-xref3,
*-------------------------------------------------------
        ldgrp LIKE bkpf-ldgrp,
*-------------------------------------------------------
        belnr LIKE bseg-belnr,
        gjahr LIKE bseg-gjahr.
DATA: color_line(4) TYPE c,
      line_color    TYPE lvc_t_scol.
DATA: edit_line     TYPE lvc_t_styl,
END OF gt_data ,
gs_data LIKE LINE OF gt_data.
*
DATA : BEGIN OF gt_belge OCCURS 0,
        belge_no(10) ,
       END OF gt_belge .

*- TYPES
TYPES: BEGIN OF ty_msg,
       status(1)    TYPE  c,
       message(255) TYPE  c,
       belnr(10),
       bukrs(4),
       gjahr(4),
       END OF ty_msg.

*-DATA
DATA:  gv_datum TYPE datum,
       gv_uzeit TYPE uzeit.
DATA : gt_msg    TYPE TABLE OF ty_msg   WITH HEADER LINE .
DATA : gt_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE .
DATA : gt_selrows TYPE lvc_t_row   .

TYPES: ty_line(2500) TYPE c.
DATA: gt_text  TYPE truxs_t_text_data.
DATA: lv_a TYPE c.
DATA: it_tab1 TYPE TABLE OF ty_line,
      it_tab2 TYPE TABLE OF ty_line,
      wa_tab  TYPE ty_line.
DATA: w_excel      TYPE ole2_object,
      w_workbook   TYPE ole2_object,
      w_worksheet  TYPE ole2_object,
      w_columns    TYPE ole2_object,
      w_column_ent TYPE ole2_object,
      w_cell       TYPE ole2_object,
      w_int        TYPE ole2_object,
      w_range      TYPE ole2_object.
DATA: w_deli(1) TYPE c, "Delimiter
      w_hex     TYPE x,
      w_rc      TYPE i.
*DATA : it_raw  TYPE truxs_t_text_data.
FIELD-SYMBOLS: <fs> .
CONSTANTS wl_c09(2) TYPE n VALUE 09.

CONSTANTS: c_ext_xls TYPE string VALUE '*.xlsx'.

*------------------------- SELECTION-SCREEN ---------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b102 WITH FRAME TITLE TEXT-002 .
SELECTION-SCREEN FUNCTION KEY 1.
PARAMETERS: p_file TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b102.
SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN: PUSHBUTTON (20) TEXT-001 USER-COMMAND cl1 MODIF ID m1.
SELECTION-SCREEN END OF LINE .
*----------------------------------------------------------------------*
