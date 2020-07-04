*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_019_OTOMATIK_ODEME_TOP
*&---------------------------------------------------------------------*
TABLES:reguh,regup,t012,bnka,lfa1,t012k,tiban,stxl,bseg.
*--------------------------------------------------------------------*
INCLUDE ole2incl.                       "include used for providing
*classes used for using create object for creating application and
*worksheets
DATA: application TYPE ole2_object,
      workbook TYPE ole2_object,
      sheet TYPE ole2_object,
      cells TYPE ole2_object,
      lo_font TYPE ole2_object,
      lo_cell TYPE ole2_object,
      font    TYPE ole2_object,
      lo_range TYPE ole2_object.
CONSTANTS: row_max TYPE i VALUE 256.
* Align:
CONSTANTS:
c_center TYPE i VALUE -4108,
c_left   TYPE i VALUE -4131,
c_right  TYPE i VALUE -4152.

DATA : v_row TYPE sy-tabix.
DATA: gt_txt TYPE TABLE OF string."text300.

*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_out ,
srno(6)  TYPE c ,
sirno TYPE i,             "sira_no
laufd TYPE reguh-laufd,   "islem_tarihi
ubnky TYPE reguh-ubnky,   "gonderen_sube_kodu
ubknt TYPE reguh-ubknt,   "gonderen_hesap_no
*uiban TYPE reguh-uiban,   "gonderen_iban
iban  TYPE tiban-iban,   "gonderen_iban
name1 TYPE reguh-name1,   "alici_adi
rwbtr TYPE reguh-rwbtr,   "tutar
waers TYPE reguh-waers,   "para_birimi
zbnkl TYPE reguh-zbnkl,   "alici_banka_kodu
abank TYPE string,        "alici_banka_adi
zbnky TYPE reguh-zbnky,   "alici_sube_kodu
asadi TYPE string,        "alici_sube_adi
zbnkn TYPE reguh-zbnkn,   "alici_hesap_no
ziban TYPE reguh-ziban,   "alici_iban
stras TYPE reguh-stras,   "alici_adres
ort01 TYPE reguh-ort01,   "alici_sehir
acklm TYPE string,        "aciklama
grefr TYPE string,        "gonderen_referans
arefr TYPE string,        "alici_referans
stcd1 TYPE reguh-stcd1,   "alici_vergi_dairesi
aveno TYPE string,        "alici_vergi_no
istip TYPE string,       "islem_tipi
lifnr TYPE reguh-lifnr,
vblnr TYPE reguh-vblnr,
  xvorl TYPE reguh-xvorl,
telf1 TYPE lfa1-telf1,
end of ty_out.

DATA: gt_out TYPE STANDARD TABLE OF ty_out,
      gs_out TYPE ty_out.
DATA: gt_reguh   TYPE TABLE OF ty_out,
      gs_reguh   TYPE ty_out.
DATA: sayac TYPE i.
DATA: lv_tarih TYPE string,
      lv_adrnr LIKE t001-adrnr,
      lv_name1 LIKE adrc-name1,
      lv_name3 LIKE adrc-name3,
      lv_toplam LIKE  reguh-rwbtr.
DATA : lv_string TYPE string ,
      lv_banka_adi TYPE bnka-banka ,
      lv_sube_adi TYPE bnka-brnch,
      lv_vergi_no TYPE lfa1-stcd2.
TYPES: BEGIN OF ty_string,
  str(25) TYPE c,
END OF ty_string.
DATA:lt_string TYPE TABLE OF ty_string,
      ls_string TYPE ty_string .
*--------------------------------------------------------------------*
DATA gr_alvgrid             TYPE REF TO cl_gui_alv_grid.
DATA gc_custom_control_name TYPE        scrfname VALUE 'CONTAINER'.
DATA gr_container           TYPE REF TO cl_gui_custom_container.
DATA gt_fieldcat            TYPE        lvc_t_fcat.
DATA:ls_fieldcat            TYPE LINE OF lvc_t_fcat.
DATA: gs_layout              TYPE        lvc_s_layo.

*--------------------------------------------------------------------*
*data for alv
DATA : g_container        TYPE scrfname VALUE 'CONTAINER',
       grid               TYPE REF TO cl_gui_alv_grid,
       g_custom_container TYPE REF TO cl_gui_custom_container,
       gs_vari100         TYPE disvariant,
       gs_layo100         TYPE lvc_s_layo.
DATA : gt_fcat TYPE lvc_t_fcat WITH HEADER LINE,
       gs_fcat TYPE lvc_s_fcat,
       gt_sort TYPE lvc_t_sort WITH HEADER LINE,
       gs_sort TYPE lvc_s_sort,
       it_rows TYPE lvc_t_row,
       gt_rows LIKE lvc_s_row OCCURS 0 WITH HEADER LINE.
DATA : gt_toolbar_excluding TYPE ui_functions  .
DATA : gt_flcat   TYPE slis_t_fieldcat_alv,
       gt_layout  TYPE slis_layout_alv,
       gt_event   TYPE slis_t_event,
       gs_variant TYPE disvariant,
       gx_variant LIKE disvariant,
       gv_save(1) TYPE c VALUE 'A'.
DATA : g_cell_color       TYPE slis_color.
DATA : wa_color_tab       TYPE slis_specialcol_alv.
DATA : i_color_tab        TYPE slis_t_specialcol_alv.
DATA : gwa_flcat LIKE LINE OF gt_flcat,
       gwa_event LIKE LINE OF gt_event,
       gwa_sort  LIKE LINE OF gt_sort.
DATA : gv_error .

DATA: ok_code100 LIKE sy-ucomm.

*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_zbukr  FOR reguh-zbukr
NO-EXTENSION NO INTERVALS OBLIGATORY DEFAULT '1000'.
*SELECT-OPTIONS : s_laufi FOR reguh-laufi NO INTERVALS.
PARAMETERS: p_laufi(5) TYPE c OBLIGATORY.
SELECT-OPTIONS : s_laufd  FOR reguh-laufd
NO INTERVALS ."Programın çalıştırma tarihi
PARAMETERS: r1 RADIOBUTTON GROUP rb1 , "gerçek çalış.
            r2 RADIOBUTTON GROUP rb1.             "öneri çalış.
SELECTION-SCREEN END OF BLOCK b1.
