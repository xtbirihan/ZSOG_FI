*&---------------------------------------------------------------------*
*& Report  ZFI_R_PESIN_ODENEN_GID
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZFI_R_PESIN_ODENEN_GID.

*<-----------------Alv definition----------------------------
TYPE-POOLS : slis, kkblo.
TABLES: zfi_pes_gid_bas, zfi_pes_gid_kal, evbs, equi, usr02, itob .
*<-----------------------------------------------------------
DATA : gv_repid    TYPE sy-repid,
       gt_fieldcat TYPE slis_t_fieldcat_alv,
       gt_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       gt_scol     TYPE slis_t_specialcol_alv, "color
       gs_scol     TYPE slis_specialcol_alv,   "color
       gs_layout   TYPE slis_layout_alv,       "color
       gt_events   TYPE slis_t_event,
       gs_events   LIKE LINE OF gt_events,
       gt_topage   TYPE slis_t_listheader.
*----------------------------------------------------------->
DATA: gv_datum TYPE sy-datum.
*DATA: gt_kayit LIKE zbtchc_t_log OCCURS 0 WITH HEADER LINE.
gv_datum = sy-datum .

DATA: gt_skat LIKE skat OCCURS 0 WITH HEADER LINE,
      gt_lfa1 LIKE lfa1 OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF gt_itab OCCURS 0,
        bukrs        LIKE zfi_pes_gid_bas-bukrs,
        belgeno      LIKE zfi_pes_gid_bas-belgeno,
        hkont        LIKE zfi_pes_gid_bas-hkont,
        hkont_txt50  LIKE skat-txt50,
        hkont_uzun   LIKE zfi_pes_gid_bas-hkont_uzun,
        hkont_gider  LIKE zfi_pes_gid_bas-hkont_gider,
        lifnr        LIKE zfi_pes_gid_bas-lifnr,
        name1        LIKE lfa1-name1,
        tarih        LIKE zfi_pes_gid_bas-tarih,
        kostl        LIKE zfi_pes_gid_bas-kostl,
        policeno     LIKE zfi_pes_gid_bas-policeno,
        vade_bas     LIKE zfi_pes_gid_bas-vade_bas,
        vade_bit     LIKE zfi_pes_gid_bas-vade_bit,
        tutar        LIKE zfi_pes_gid_bas-tutar,
        kursf        LIKE zfi_pes_gid_bas-kursf,
        tutar_upb    LIKE bseg-dmbtr,
        tutar_net    LIKE bseg-dmbtr,
*  tutar_upb     LIKE zfi_pes_gid_bas-tutar,
*  tutar_risk    like zfi_pes_gid_bas-tutar_risk,
        waers        LIKE zfi_pes_gid_bas-waers,
        sgtxt        LIKE zfi_pes_gid_bas-sgtxt,
        awkey        LIKE zfi_pes_gid_bas-awkey,
        durum        LIKE zfi_pes_gid_bas-durum,
*  quarter(2)    TYPE c,
        spmon        LIKE zfi_pes_gid_kal-spmon,
        taksitno     LIKE zfi_pes_gid_kal-taksitno,
        gun          LIKE zfi_pes_gid_kal-gun,
        tutar_taksit LIKE zfi_pes_gid_kal-tutar,
        valor        LIKE zfi_pes_gid_kal-valor,
        muaf_tutar   LIKE zfi_pes_gid_bas-muaf_tutar,
        temin_tutar  LIKE zfi_pes_gid_bas-temin_tutar,
        police_turu  LIKE zfi_pes_gid_bas-police_turu,
        police_konu  LIKE zfi_pes_gid_bas-police_konu,
        awkey_vuk    LIKE zfi_pes_gid_kal-awkey_vuk,
        awkey_tfrs   LIKE zfi_pes_gid_kal-awkey_tfrs,
        awkey_vrm    LIKE zfi_pes_gid_kal-awkey_vrm,
      END OF gt_itab.

DATA: gs_itab LIKE gt_itab.

SELECTION-SCREEN: BEGIN OF BLOCK blk1.
SELECT-OPTIONS: so_bukrs FOR zfi_pes_gid_bas-bukrs MEMORY ID buk,
                so_tarih FOR zfi_pes_gid_bas-tarih,
                so_spmon FOR zfi_pes_gid_kal-spmon,
                so_belge FOR zfi_pes_gid_bas-belgeno,
                so_polic FOR zfi_pes_gid_bas-policeno,
                so_hkont FOR zfi_pes_gid_bas-hkont.
SELECTION-SCREEN: END OF BLOCK blk1.

SELECTION-SCREEN ULINE.

PARAMETERS:
  rb1 RADIOBUTTON GROUP gr1,
  rb2 RADIOBUTTON GROUP gr1.
*  rb3 radiobutton group gr1.

LOAD-OF-PROGRAM.
*  p_laeda = sy-datum.

START-OF-SELECTION.
  PERFORM initial_data.
  PERFORM get_data.

END-OF-SELECTION.
  PERFORM write_data.
*&---------------------------------------------------------------------*
*&      Form  INITIAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initial_data.
  REFRESH gt_itab. CLEAR gt_itab.
  gv_repid = sy-repid.
ENDFORM.                    " INITIAL_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lt_line TYPE zfi_pes_gid_kal OCCURS 0 WITH HEADER LINE .

  IF rb1 = 'X'.
    SELECT
      bukrs
      hkont
      tarih
      belgeno
      lifnr
      policeno
      sgtxt
      vade_bas
      vade_bit
      tutar
      kursf
*      tutar_risk
      waers
      awkey
      durum
      muaf_tutar
      temin_tutar
      police_turu
      police_konu
   INTO CORRESPONDING FIELDS OF TABLE gt_itab
   FROM zfi_pes_gid_bas
   WHERE bukrs    IN so_bukrs
     AND tarih    IN so_tarih
     AND belgeno  IN so_belge
     AND policeno IN so_polic
     AND durum    NE 'I'.

    SELECT * FROM zfi_pes_gid_kal
      INTO CORRESPONDING FIELDS OF TABLE lt_line
      FOR ALL ENTRIES IN gt_itab
      WHERE belgeno  EQ gt_itab-belgeno.

    SELECT *
      FROM lfa1
      INTO TABLE gt_lfa1
      FOR ALL ENTRIES IN gt_itab
      WHERE lifnr EQ gt_itab-lifnr.

    SELECT *
          FROM skat
          INTO TABLE gt_skat
          FOR ALL ENTRIES IN gt_itab
          WHERE ktopl = 'SGHP' AND spras = 'TR'
          AND saknr EQ gt_itab-hkont.
    SORT gt_skat BY saknr.
    SORT gt_itab BY lifnr.
    LOOP AT gt_itab INTO gs_itab.

      LOOP AT lt_line
        WHERE belgeno EQ gs_itab-belgeno .
        gs_itab-tutar_net = gs_itab-tutar_net + lt_line-tutar.
      ENDLOOP.

      IF gs_itab-waers EQ 'TRY' .
        gs_itab-tutar_upb = gs_itab-tutar .
      ELSE.
        gs_itab-tutar_upb = gs_itab-tutar * gs_itab-kursf.
        gs_itab-tutar_net = gs_itab-tutar_net * gs_itab-kursf.
      ENDIF.

      READ TABLE gt_lfa1 WITH KEY lifnr = gs_itab-lifnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_itab-name1 = gt_lfa1-name1.
      ENDIF.

      READ TABLE gt_skat WITH KEY saknr = gs_itab-hkont BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_itab-hkont_txt50 = gt_skat-txt20.
      ENDIF.

      MODIFY gt_itab FROM gs_itab.

*    gt_itab-quarter = gt_itab-spmon+4(2).

    ENDLOOP.

  ELSEIF rb2 = 'X'.

    SELECT
        a~bukrs
        a~belgeno
        a~hkont
        a~hkont_uzun
        a~hkont_gider
        a~lifnr
        a~tarih
        a~kostl
        a~policeno
        a~vade_bas
        a~vade_bit
        a~tutar
        a~waers
        a~sgtxt
        a~awkey
        a~durum
        a~muaf_tutar
        a~temin_tutar
        a~police_turu
        a~police_konu
        b~spmon
        b~taksitno
        b~gun
        b~tutar
        a~kursf
        b~valor
        b~awkey_vuk
        b~awkey_tfrs
        b~awkey_vrm
INTO CORRESPONDING FIELDS OF TABLE gt_itab
      FROM zfi_pes_gid_bas AS a INNER JOIN zfi_pes_gid_kal AS b
      ON a~belgeno = b~belgeno
      WHERE a~bukrs IN so_bukrs
      AND a~tarih IN so_tarih
      AND b~spmon IN so_spmon
      AND a~hkont IN so_hkont
      and a~durum NE 'I'.


    SELECT *
      FROM skat
      INTO TABLE gt_skat
      FOR ALL ENTRIES IN gt_itab
      WHERE ktopl = 'SGHP' AND spras = 'TR'
      AND saknr EQ gt_itab-hkont.
    SORT gt_skat BY saknr.
    LOOP AT gt_itab INTO gs_itab.

      READ TABLE gt_skat WITH KEY saknr = gs_itab-hkont BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_itab-hkont_txt50 = gt_skat-txt20.
      ENDIF.


      IF gs_itab-waers EQ 'TRY'.
        gs_itab-kursf       = 1 .
        gs_itab-tutar_upb   = gs_itab-tutar .
      ELSE.
        gs_itab-tutar_upb   = gs_itab-tutar * gs_itab-kursf .
      ENDIF.


      MODIFY gt_itab FROM gs_itab.

*    gt_itab-quarter = gt_itab-spmon+4(2).

    ENDLOOP.

  ENDIF.

*  SELECT s~belnr d~kunnr s~gjahr s~buzei k~name1
*  INTO CORRESPONDING FIELDS OF TABLE itab
*  FROM  ( ( bsid as d
*  inner join bsis as s on d~belnr = s~belnr
*  and d~bukrs = s~bukrs
*  and d~gjahr = s~gjahr
*  and d~buzei = s~buzei )
*  inner join kna1 as k on k~kunnr = d~kunnr
*  and k~kunnr = p_kunnr ).
*
*  SELECT *
*    FROM mara
*    INTO CORRESPONDING FIELDS OF TABLE gt_itab
*      UP TO 100 ROWS
*   WHERE matnr IN so_matnr
*     AND ernam IN so_ernam
*     AND mtart IN so_mtart
*     AND laeda IN so_laeda.
  IF sy-subrc EQ 0.

  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_data .
  PERFORM prepare_fieldcat_alv.
  PERFORM prepare_layout_alv.
  PERFORM prepare_events_alv.
  PERFORM color_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort[]
      i_default                = 'X'
      i_save                   = 'A'
      it_events                = gt_events[]
    TABLES
      t_outtab                 = gt_itab.
ENDFORM.                    " WRITE_DATA
*&---------------------------------------------------------------------
*&      Form  prepare_layout_alv
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
FORM prepare_layout_alv .
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra             = 'X'.
  gs_layout-no_input          = 'X'.
ENDFORM.                    " prepare_layout_alv
*&---------------------------------------------------------------------
*&      Form  prepare_events_alv
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
FORM prepare_events_alv .
  DATA: ls_event TYPE slis_alv_event.

* Get events supported by ALV
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events.
* User Command
  READ TABLE gt_events WITH KEY name = 'USER_COMMAND'
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'UCOMM_ALV' TO ls_event-form.
    MODIFY gt_events FROM ls_event INDEX sy-tabix .
  ENDIF.

ENDFORM.                    " prepare_events_alv
*&---------------------------------------------------------------------
*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
*  SET PF-STATUS 'STANDARD_FULLSCREEN'.
  SET TITLEBAR 'TITLE1' .
ENDFORM.                    "SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page .
  DATA: ls_top      LIKE LINE OF gt_topage,
        lv_str(200) TYPE c,
        lv_line     TYPE i.
  REFRESH gt_topage.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_topage.
ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  color_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM color_alv .
  DATA: lv_color1 TYPE i,
        lv_color2 TYPE i.
  lv_color1 = 2.
  lv_color2 = 4.

ENDFORM.                    " color_alv
*&---------------------------------------------------------------------
*
*&      Form  prepare_fieldcat_alv
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------*
FORM prepare_fieldcat_alv.
  DATA: ls_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

* Get field names of structure or internal table used in fieldcatalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = gv_repid
      i_internal_tabname     = 'GT_ITAB'
      i_inclname             = gv_repid
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

* Set field properties

  IF rb1 = 'X'.
    PERFORM baslik.
  ELSEIF rb2 = 'X'.
    PERFORM detay.
  ENDIF.

  CLEAR: gt_sort, gt_sort[].

ENDFORM.                    " prepare_fieldcat_alv
*&---------------------------------------------------------------------*
*&      Form  UCOMM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ucomm_alv USING ip_ucomm    LIKE sy-ucomm
                     ip_selfield TYPE kkblo_selfield.
  CASE ip_ucomm.
    WHEN 'BILGI'.
*      PERFORM
    WHEN '&IC1' .
      IF ip_selfield-fieldname EQ 'AWKEY' OR
         ip_selfield-fieldname EQ 'AWKEY_TFRS' OR
         ip_selfield-fieldname EQ 'AWKEY_VUK' OR
         ip_selfield-fieldname EQ 'AWKEY_VRM'.
        SET PARAMETER ID 'BUK' FIELD ip_selfield-value+10(4) .
        SET PARAMETER ID 'BLN' FIELD ip_selfield-value(10) .
        SET PARAMETER ID 'GJR' FIELD ip_selfield-value+14(4) .
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
      ENDIF .
    WHEN OTHERS.
  ENDCASE.

ENDFORM .                    "UCOMM_ALV
*&---------------------------------------------------------------------*
*&      Form  BASLIK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM baslik .

  DATA: ls_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  LOOP AT gt_fieldcat INTO ls_fieldcat .
    CASE ls_fieldcat-fieldname .
      WHEN 'BUKRS'.
        ls_fieldcat-seltext_l    = 'Şirket kodu'.
        ls_fieldcat-outputlen    = 9.
        ls_fieldcat-col_pos      = 1.
*        ls_fieldcat-icon         = 'X'.

      WHEN 'BELGENO'.
        ls_fieldcat-seltext_l    = 'Sistem Referans No' .
        ls_fieldcat-outputlen    = 5.
        ls_fieldcat-col_pos      = 2.

      WHEN 'POLICENO'.
        ls_fieldcat-seltext_l    = 'Poliçe Matbuu No' .
        ls_fieldcat-outputlen    = 5.
        ls_fieldcat-col_pos      = 2.

      WHEN 'TARIH'.
        ls_fieldcat-seltext_l    = 'Poliçe Tarihi' .
        ls_fieldcat-outputlen    = 5.
        ls_fieldcat-col_pos      = 2.

      WHEN 'HKONT'.
        ls_fieldcat-seltext_l    = 'Hesap Kodu (180)' .
        ls_fieldcat-outputlen    = 12.
        ls_fieldcat-col_pos      = 3.

      WHEN 'HKONT_TXT50'.
        ls_fieldcat-seltext_l    = 'Hesap Adı (180)' .
        ls_fieldcat-outputlen    = 5.
        ls_fieldcat-col_pos      = 4.

      WHEN 'LIFNR'.
        ls_fieldcat-seltext_l    = 'Satıcı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 5.

      WHEN 'NAME1'.
        ls_fieldcat-seltext_l    = 'Adı'.
        ls_fieldcat-outputlen    = 35.
        ls_fieldcat-col_pos      = 6.

      WHEN 'XBLNR'.
        ls_fieldcat-seltext_l    = 'Referans'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 7.

      WHEN 'SGTXT'.
        ls_fieldcat-seltext_l    = 'Metin'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'VADE_BAS'.
        ls_fieldcat-seltext_l    = 'Vade Başlangıcı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 9.

      WHEN 'VADE_BIT'.
        ls_fieldcat-seltext_l    = 'Vade Bitişi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 10.

      WHEN 'TUTAR'.
        ls_fieldcat-seltext_l    = 'Toplam Poliçe Tutarı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 11.

      WHEN 'TUTAR_UPB'.
        ls_fieldcat-seltext_l    = 'Top. Poliçe Tut. (TRY) '.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 11.

      WHEN 'TUTAR_NET'.
        ls_fieldcat-seltext_l    = 'Net. Poliçe Tut. (TRY) '.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 11.

      WHEN 'WAERS'.
        ls_fieldcat-seltext_l    = 'Para Birimi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 12.

      WHEN 'TUTAR_RISK'.
        ls_fieldcat-seltext_l    = 'Risk Tutarı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 13.

      WHEN 'DURUM'.
        ls_fieldcat-seltext_l    = 'Poliçe Durum'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 13.

      WHEN 'AWKEY'.
        ls_fieldcat-seltext_l    = 'FI Belge No'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 13.

      WHEN OTHERS.
        ls_fieldcat-no_out       = 'X'.

    ENDCASE.
    ls_fieldcat-seltext_m    = ls_fieldcat-seltext_l.
    ls_fieldcat-seltext_s    = ls_fieldcat-seltext_l.
    ls_fieldcat-reptext_ddic = ls_fieldcat-seltext_l.
    MODIFY gt_fieldcat FROM ls_fieldcat .
  ENDLOOP.
ENDFORM.                    " BASLIK
*&---------------------------------------------------------------------*
*&      Form  DETAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detay .
  DATA: ls_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
  LOOP AT gt_fieldcat INTO ls_fieldcat .
    CASE ls_fieldcat-fieldname .
      WHEN 'BUKRS'.
        ls_fieldcat-seltext_l    = 'Şirket kodu'.
        ls_fieldcat-outputlen    = 4.
        ls_fieldcat-col_pos      = 1.
*        ls_fieldcat-icon         = 'X'.

      WHEN 'BELGENO'.
        ls_fieldcat-seltext_l    = 'Poliçe Referans' .
        ls_fieldcat-outputlen    = 5.
        ls_fieldcat-col_pos      = 2.

      WHEN 'POLICENO'.
        ls_fieldcat-seltext_l    = 'Fatura No' .
        ls_fieldcat-outputlen    = 5.
        ls_fieldcat-col_pos      = 2.

      WHEN 'SPMON'.
        ls_fieldcat-seltext_l    = 'Dönem'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 3.

      WHEN 'HKONT'.
        ls_fieldcat-seltext_l    = 'Hesap Kodu (180)' .
        ls_fieldcat-outputlen    = 12.
        ls_fieldcat-col_pos      = 3.

      WHEN 'HKONT_TXT50'.
        ls_fieldcat-seltext_l    = 'Hesap Adı (180)' .
        ls_fieldcat-outputlen    = 5.
        ls_fieldcat-col_pos      = 4.

      WHEN 'HKONT_UZUN'.
        ls_fieldcat-seltext_l    = 'Hesap Kodu (280)'.
        ls_fieldcat-outputlen    = 12.
        ls_fieldcat-col_pos      = 5.

      WHEN 'HKONT_GIDER'.
        ls_fieldcat-seltext_l    = 'Hesap Kodu (Gider)'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 6.

      WHEN 'TARIH'.
        ls_fieldcat-seltext_l    = 'Poliçe Tarihi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 7.

      WHEN 'KOSTL'.
        ls_fieldcat-seltext_l    = 'Masraf Yeri'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'XBLNR'.
        ls_fieldcat-seltext_l    = 'Referans'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'VADE_BAS'.
        ls_fieldcat-seltext_l    = 'Vade Başlangıcı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'VADE_BIT'.
        ls_fieldcat-seltext_l    = 'Vade Bitişi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'TUTAR'.
        ls_fieldcat-seltext_l    = 'Toplam Tutar (Belge Para Br.)'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'WAERS'.
        ls_fieldcat-seltext_l    = 'Para Birimi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'KURSF'.
        ls_fieldcat-seltext_l    = 'Çeviri Kuru'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'TUTAR_UPB'.
        ls_fieldcat-seltext_l    = 'Toplam Tutar (UPB)'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'SGTXT'.
        ls_fieldcat-seltext_l    = 'Metin'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'AWKEY'.
        ls_fieldcat-seltext_l    = 'Belge No'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'TAKSITNO'.
        ls_fieldcat-seltext_l    = 'Taksit No'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'GUN'.
        ls_fieldcat-seltext_l    = 'Gün sayısı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'TUTAR_TAKSIT'.
        ls_fieldcat-seltext_l    = 'Taksit Tutarı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'VALOR'.
        ls_fieldcat-seltext_l    = 'Giderleştirme Tarihi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'MUAF_TUTAR'.
        ls_fieldcat-seltext_l    = 'Poliçe Muafiyet Tutarı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'TEMIN_TUTAR'.
        ls_fieldcat-seltext_l    = 'Poliçe Teminat Tutarı'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'POLICE_TURU'.
        ls_fieldcat-seltext_l    = 'Poliçe Türü'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'POLICE_KONU'.
        ls_fieldcat-seltext_l    = 'Poliçe Konusu'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'AWKEY_VUK'.
        ls_fieldcat-seltext_l    = 'VUK Virman belgesi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'AWKEY_TFRS'.
        ls_fieldcat-seltext_l    = 'TFRS Virman belgesi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN 'AWKEY_VRM'.
        ls_fieldcat-seltext_l    = '12 Aylık VUK Virman belgesi'.
        ls_fieldcat-outputlen    = 13.
        ls_fieldcat-col_pos      = 8.

      WHEN OTHERS.
        ls_fieldcat-no_out       = 'X'.

    ENDCASE.
    ls_fieldcat-seltext_m    = ls_fieldcat-seltext_l.
    ls_fieldcat-seltext_s    = ls_fieldcat-seltext_l.
    ls_fieldcat-reptext_ddic = ls_fieldcat-seltext_l.
    MODIFY gt_fieldcat FROM ls_fieldcat .
  ENDLOOP.
ENDFORM.                    " DETAY
