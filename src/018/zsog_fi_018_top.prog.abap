*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_018_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS: esp1.
DATA: BEGIN OF gs_scr_1903.
DATA: ucomm              TYPE sy-ucomm,
      sum_alv            TYPE TABLE OF ZSOG_FI_018_S_01, "satıcı bazında özetlenmiş alv
      auth               TYPE ZSOG_FI_018_T_03,
      detail             TYPE TABLE OF ZSOG_FI_018_S_02
                         WITH NON-UNIQUE SORTED KEY lifnr COMPONENTS lifnr,
      kismi              TYPE TABLE OF ZSOG_FI_018_S_004
                         WITH NON-UNIQUE SORTED KEY lifnr COMPONENTS lifnr,
      kismi_diger        TYPE TABLE OF ZSOG_FI_018_S_004
                         WITH NON-UNIQUE SORTED KEY lifnr COMPONENTS lifnr,
      filter             TYPE TABLE OF ZSOG_FI_018_S_02
                         WITH NON-UNIQUE SORTED KEY lifnr COMPONENTS lifnr,
      onay_durum         TYPE ZSOG_FI_018_T_01,
      category           TYPE ZSOG_FI_018_S_003,
      header_tab         TYPE TABLE OF ZSOG_FI_018_T_01,
      tanitici           TYPE  ZSOG_FI_018_T_01,
      item_tab           TYPE TABLE OF ZSOG_FI_018_T_04,
      r_alv              TYPE REF TO cl_salv_table,
      r_columns          TYPE REF TO cl_salv_columns_table,
      r_column           TYPE REF TO cl_salv_column,
      r_events           TYPE REF TO cl_salv_events_table,
      r_selections       TYPE REF TO cl_salv_selections,
      name_of_alv(30),
      r_grid1            TYPE REF TO cl_gui_alv_grid,
      r_custom_container TYPE REF TO cl_gui_custom_container,
      s_layout           TYPE lvc_s_layo,
      t_exclude          TYPE ui_functions,
      t_fieldcat         TYPE lvc_t_fcat.
DATA: END OF gs_scr_1903.
DATA: gv_container        TYPE scrfname VALUE 'CONT1'.
DATA: gt_bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
DATA: gt_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.


*DATA: t_rettab  TYPE TABLE OF ddshretval.

  DATA: BEGIN OF gs_header OCCURS 0,
          bukrs        LIKE bsik-bukrs,
          lifnr        LIKE bsik-lifnr,
          satici_adi   LIKE lfa1-name1,
          land1        LIKE lfa1-land1,
          laufd        LIKE zsog_fi_018_t_01-laufd,
          laufi        LIKE zsog_fi_018_t_01-laufi,
          kunnr        LIKE knb1-kunnr,
          satici_grubu LIKE lfa1-ktokk,
*          sektor       LIKE zsog_fi_018_t_01-sektor,"ozans
          wrbtr        LIKE bsik-wrbtr,
          kdvli_tut    LIKE bsik-wrbtr,
          kdvsiz_tut   LIKE bsik-wrbtr,
          kdv_tut      LIKE bsik-wrbtr,
        END OF gs_header.

" kısmi ve detay alv'nin hotspotunda satıcı tespit edilmesi için
  DATA: gv_lifnr LIKE lfa1-lifnr.

*{   ->>> Inserted by Prodea Ozan Şahin - 22.06.2020 21:33:00
  " Hepsini dağıt butonu kalan tutar
dATA: BEGIN OF gs_dagit,
       oneri TYPE bsik-wrbtr,
       flag  TYPE c,
     END OF gs_dagit.
*}     <<<- End of   Inserted - 22.06.2020 21:33:00
