*&---------------------------------------------------------------------*
*&  Include           ZFI_I_PESIN_ODENEN_GID_TOP
*&---------------------------------------------------------------------*

TABLES : bkpf , zfi_pes_gid_bas , zfi_pes_gid_kal.

DATA : ok_code TYPE ok .

DATA : BEGIN OF gt_upload OCCURS 0 ,
        tarih(10) ,
        policeno LIKE zfi_pes_gid_bas-policeno ,
        hkont LIKE SKA1-saknr ,
        hkont_uzun LIKE SKb1-saknr,"bsid-hkont ,
        hkont_gider LIKE bsak-hkont,"bsid-hkont ,
        bktxt LIKE bkpf-bktxt ,
        kostl LIKE bsid-kostl ,
        gsber LIKE bsid-gsber ,
        vade_bas(10) ,
        vade_bit(10) ,
        tutar(30) ,
        mwskz LIKE bsid-mwskz ,
        kunnr LIKE bsid-kunnr,
        lifnr LIKE bsik-lifnr ,
        aufnr  LIKE bsik-aufnr,
        anahesap LIKE bsik-hkont,
        sgtxt LIKE bsid-sgtxt ,
       END OF gt_upload .

DATA : BEGIN OF gt_upl_exc OCCURS 0 ,
        policeno LIKE zfi_pes_gid_bas-policeno ,
        tarih LIKE zfi_pes_gid_bas-tarih ,
        hkont LIKE bsid-hkont ,
        hkont_uzun LIKE bsid-hkont ,
        hkont_gider LIKE bsid-hkont ,
        bktxt LIKE bkpf-bktxt ,
        kostl LIKE bsid-kostl ,
        gsber LIKE bsid-gsber ,
        vade_bas LIKE zfi_pes_gid_bas-vade_bas ,
        vade_bit LIKE zfi_pes_gid_bas-vade_bit ,
        tutar LIKE zfi_pes_gid_bas-tutar ,
        mwskz LIKE bsid-mwskz ,
        kunnr LIKE bsid-kunnr,
        lifnr LIKE bsik-lifnr ,
        aufnr LIKE bsik-aufnr,
        anahesap LIKE bsik-hkont,
        taksit LIKE bsik-zbd1t ,
        matnr LIKE bseg-matnr ,
        menge LIKE mseg-menge ,
        meins LIKE mara-meins ,
        sgtxt LIKE bsid-sgtxt ,
       END OF gt_upl_exc .

DATA : BEGIN OF gt_data OCCURS 0 ,
        counter LIKE sy-index ,
        policeno LIKE zfi_pes_gid_bas-policeno ,
        tarih LIKE bkpf-budat ,
        hkont LIKE bsid-hkont ,
        hkont_uzun  LIKE bsid-hkont ,
        hkont_gider LIKE bsid-hkont ,
*        hkont LIKE zfi_pes_gid_bas-hkont , " bsid-hkont ,
*        hkont_uzun  LIKE zfi_pes_gid_bas-hkont_uzun ," bsid-hkont ,
*        hkont_gider LIKE zfi_pes_gid_bas-hkont_gider , "bsid-hkont ,
        bktxt LIKE bkpf-bktxt ,
        kostl LIKE bsid-kostl ,
        gsber LIKE bsid-gsber ,
        vade_bas LIKE bsid-zfbdt ,
        vade_bit LIKE bsid-zfbdt ,
        tutar LIKE bsid-dmbtr ,
        mwskz LIKE bsid-mwskz ,
        kursf LIKE zfi_pes_gid_bas-kursf ,
        kunnr LIKE bsid-kunnr,
        lifnr LIKE bsik-lifnr ,
        aufnr  LIKE bsid-aufnr,
        anahesap LIKE bsik-hkont,
        taksit LIKE bsik-zbd1t ,
        muaf_tutar  LIKE zfi_pes_gid_bas-muaf_tutar ,
        temin_tutar LIKE zfi_pes_gid_bas-temin_tutar ,
        police_turu LIKE zfi_pes_gid_bas-police_turu ,
        police_konu LIKE zfi_pes_gid_bas-police_konu ,
        matnr LIKE bseg-matnr ,
        menge LIKE mseg-menge ,
        meins LIKE mara-meins ,
        sgtxt LIKE bsid-sgtxt ,
        awkey LIKE bkpf-awkey ,
        message LIKE bapiret2-message ,
        virman_awkey   LIKE bkpf-awkey ,
        virman_message LIKE bapiret2-message ,
        belgeno LIKE zfi_pes_gid_bas-belgeno ,
        rowcolor(4) ,
        zterm like lfb1-zterm,
       END OF gt_data .
DATA : wa_data LIKE LINE OF gt_data .
DATA : filename LIKE rlgrap-filename .
DATA : container TYPE REF TO cl_gui_custom_container .
DATA : alvgrid TYPE REF TO cl_gui_alv_grid .
DATA : virman_tutari TYPE dmbtr .
DATA : virman_belgesi LIKE bkpf-awkey ,
       virman_message LIKE bapiret2-message .
DATA : BEGIN OF it_skat OCCURS 0 ,
        saknr LIKE skat-saknr ,
        txt20 LIKE skat-txt20 ,
       END OF it_skat .
DATA : saticitutar   TYPE dmbtr ,
       musteritutar  TYPE dmbtr,
       anahesaptutar TYPE dmbtr.
DATA : e_valid VALUE 'X' .
DATA : ls_taksit LIKE zfi_taksit .
*data : gt_taksit like ZFI_TAKSIT OCCURS 0 WITH HEADER LINE .
DATA : BEGIN OF gt_taksit_tum OCCURS 0 .
       INCLUDE STRUCTURE zfi_taksit .
DATA : field_style TYPE lvc_t_styl .
DATA : END OF gt_taksit_tum .
DATA : it_style_kapali TYPE lvc_t_styl .
DATA : wa_style LIKE LINE OF it_style_kapali .
DATA : BEGIN OF gt_taksit OCCURS 0 .
       INCLUDE STRUCTURE zfi_taksit .
DATA : field_style TYPE lvc_t_styl .
DATA : END OF gt_taksit  .
DATA : BEGIN OF it_policeno OCCURS 0 ,
        policeno LIKE zfi_pes_gid_bas-policeno ,
       END OF it_policeno .
DATA : lv_begda LIKE sy-datum ,
       lv_endda LIKE sy-datum .
DATA : itemno(3) TYPE n  .
DATA : begyear LIKE bsid-gjahr .
DATA : toplamgun TYPE i .
DATA : kalantutar LIKE bsid-dmbtr .
DATA : vergitutari LIKE bsid-dmbtr .
DATA : vergihesabi LIKE bsid-hkont .
DATA : BEGIN OF it_vergi OCCURS 0 ,
        kalantutar LIKE bsid-dmbtr ,
        vergitutari LIKE bsid-dmbtr ,
        vergihesabi LIKE bsid-hkont ,
        ktosl       LIKE bseg-ktosl ,
       END OF it_vergi .
DATA : matrah LIKE bsid-dmbtr .
DATA : ktosl LIKE t683s-kvsl1 .
DATA : BEGIN OF it_log OCCURS 0 ,
        policeno LIKE zfi_pes_gid_bas-policeno ,
        sayac LIKE sy-index ,
        lifnr LIKE lfa1-lifnr ,
        kunnr like kna1-kunnr,
        budat LIKE bsid-budat ,
        awkey LIKE bkpf-awkey ,
        message LIKE bapiret2-message ,
        virman_awkey   LIKE bkpf-awkey ,
        virman_message LIKE bapiret2-message ,
       END OF it_log .
DATA :extab TYPE slis_t_extab WITH HEADER LINE .
DATA : lv_index LIKE sy-index .
DATA : documentheader LIKE bapiache09,
       it_accountgl      LIKE TABLE OF bapiacgl09 WITH HEADER LINE ,
       it_accountpayable LIKE TABLE OF bapiacap09 WITH HEADER LINE ,
       it_accountreceivable LIKE TABLE OF bapiacar09 WITH HEADER LINE,
       it_currencyamount LIKE TABLE OF bapiaccr09 WITH HEADER LINE ,
       it_accounttax     LIKE TABLE OF bapiactx09 WITH HEADER LINE ,
       it_return         LIKE TABLE OF bapiret2   WITH HEADER LINE ,
       it_accountwt      LIKE TABLE OF bapiacwt09 WITH HEADER LINE .
DATA : wa_bkpf LIKE bkpf ,
       it_bseg LIKE bseg OCCURS 0 WITH HEADER LINE .
DATA : it_lfbw LIKE lfbw OCCURS 0 WITH HEADER LINE .
DATA : selrows TYPE  lvc_t_row WITH HEADER LINE .

DATA: BEGIN OF gs_zmasr_hesap,
         hkont TYPE zfi_masr_hesap-hkont,
         kosar TYPE zfi_masr_hesap-kosar,
      END OF gs_zmasr_hesap,
      gt_zmasr_hesap LIKE TABLE OF gs_zmasr_hesap.

DATA: BEGIN OF gs_aufk,
        aufnr TYPE aufk-aufnr,
        bukrs TYPE aufk-bukrs,
        kokrs TYPE aufk-kokrs,
        cycle TYPE aufk-cycle,
      END OF gs_aufk,
      gt_aufk LIKE TABLE OF gs_aufk.

DATA: BEGIN OF gs_skb1,
        bukrs TYPE skb1-bukrs,
        saknr TYPE skb1-saknr,
        fstag TYPE skb1-fstag,
      END OF gs_skb1,
      gt_skb1 LIKE TABLE OF gs_skb1.

DATA: gv_belgeno TYPE zfi_pes_gid_kal-belgeno.


"ALV 1
"OBJECT ORIENTED ALV TANIMLARI
DATA : g_container         TYPE scrfname VALUE 'GRID',
       grid                TYPE REF TO cl_gui_alv_grid,
       g_custom_container  TYPE REF TO cl_gui_custom_container,
       gs_vari100          TYPE disvariant,
       gs_layo100          TYPE lvc_s_layo.
DATA : gt_fcat             TYPE lvc_t_fcat WITH HEADER LINE,
       gs_fcat             TYPE lvc_s_fcat,
       gt_sort             TYPE lvc_t_sort WITH HEADER LINE,
       gs_sort             TYPE lvc_s_sort,
       it_rows             TYPE lvc_t_row ,
       gt_rows             LIKE lvc_s_row OCCURS 0 WITH HEADER LINE.

"ALV TANIMLARI
DATA: gt_flcat   TYPE slis_t_fieldcat_alv,
      gt_layout  TYPE slis_layout_alv,
      gt_event   TYPE slis_t_event,
*      gs_variant TYPE disvariant,
*      gx_variant LIKE disvariant,
      gv_save(1) TYPE c VALUE 'A' .

DATA :v_alv_variant        TYPE disvariant.
DATA :gt_toolbar_excluding TYPE ui_functions.

DATA: gwa_flcat LIKE LINE OF gt_flcat,
      gwa_event LIKE LINE OF gt_event,
      gwa_sort  LIKE LINE OF gt_sort.

"ALV 2
"OBJECT ORIENTED ALV TANIMLARI
DATA : g_container2         TYPE scrfname VALUE 'GRID2',
       grid2                TYPE REF TO cl_gui_alv_grid,
       g_custom_container2  TYPE REF TO cl_gui_custom_container,
       gs_vari1002          TYPE disvariant,
       gs_layo1002          TYPE lvc_s_layo.
DATA : gt_fcat2             TYPE lvc_t_fcat WITH HEADER LINE,
       gs_fcat2             TYPE lvc_s_fcat,
       gt_sort2             TYPE lvc_t_sort WITH HEADER LINE,
       gs_sort2             TYPE lvc_s_sort,
       it_rows2             TYPE lvc_t_row ,
       gt_rows2             LIKE lvc_s_row OCCURS 0 WITH HEADER LINE.

"ALV TANIMLARI
DATA: gt_flcat2   TYPE slis_t_fieldcat_alv,
      gt_layout2  TYPE slis_layout_alv,
      gt_event2   TYPE slis_t_event,
*      gs_variant TYPE disvariant,
*      gx_variant LIKE disvariant,
      gv_save2(1) TYPE c VALUE 'A' .

DATA :v_alv_variant2        TYPE disvariant.
DATA :gt_toolbar_excluding2 TYPE ui_functions.

DATA: gwa_flcat2 LIKE LINE OF gt_flcat,
      gwa_event2 LIKE LINE OF gt_event,
      gwa_sort2  LIKE LINE OF gt_sort.


DATA: gv_okcode200 TYPE sy-ucomm.

DATA : gt_200 LIKE gt_data OCCURS 0 WITH HEADER LINE,
       gs_200_baslik LIKE zfi_pes_gid_bas ,
       gt_200_kalem LIKE zfi_pes_gid_kal OCCURS 0 WITH HEADER LINE.
DATA: gv_tarih_200      TYPE sy-datum.
DATA: gv_tarih_200_post TYPE sy-datum.
DATA : gv_dummy TYPE c ,
       gt_message TYPE TABLE OF bapiret2 WITH HEADER LINE .

DATA: BEGIN OF gt_200_belge OCCURS 0.
INCLUDE STRUCTURE zfi_pes_gid_kal.
DATA: tutar_son LIKE BSEG-dmbtr,
      shkzg_son LIKE bseg-shkzg,
      kalem     TYPE c,
      tip       TYPE c.
DATA: END OF gt_200_belge.

DATA: gt_bseg TYPE bseg OCCURS 0 WITH HEADER LINE.
DATA: gs_f4 TYPE lvc_s_f4,
      gt_f4 TYPE lvc_t_f4.

  """" EVENT HANDELER CLASS

  CLASS lcl_event_handler DEFINITION.
    PUBLIC SECTION.
      METHODS:
      handle_double_click
            FOR EVENT double_click OF cl_gui_alv_grid
                IMPORTING e_row e_column." ,
*
*      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
*        IMPORTING sender
*        e_fieldname
*        e_fieldvalue
*        es_row_no
*        er_event_data
*        et_bad_cells
*        e_display.

  ENDCLASS.                    "lcl_event_handler1 DEFINITION

  CLASS lcl_event_handler IMPLEMENTATION.

*METHOD on_f4.
* FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.
*
*  DATA: lt_ret          TYPE TABLE OF ddshretval.
*  DATA: BEGIN OF ls_data ,
*        bukrs TYPE skb1-bukrs,
*        saknr TYPE skb1-saknr,
*        END OF ls_data,
*        lt_data         like TABLE OF ls_data,
*        ls_sel          LIKE LINE OF lt_ret,
*        ls_modi         TYPE lvc_s_modi,
*        field_tab       TYPE TABLE OF dfies,
*        dynpfld_mapping TYPE TABLE OF dselc.
*
**
***  READ TABLE gt_popup01 INTO DATA(ls_popup) INDEX p_es_row_no-row_id.
***  CHECK sy-subrc = 0.
***
*  SELECT bukrs
*         saknr
*    FROM skb1
*    INTO TABLE lt_data
*    WHERE bukrs = '2425'.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'HKONT'
*      dynpprog        = sy-repid
*      dynpnr          = sy-dynnr
*      window_title    = 'List of State entries'(002)
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_data[]
*      field_tab       = field_tab[]
*      return_tab      = lt_ret[]
*      dynpfld_mapping = dynpfld_mapping[]
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
**
**  READ TABLE lt_ret INTO ls_sel INDEX 1.
**  ASSIGN p_er_event_data->m_data->* TO <itab>.
**  READ TABLE gt_popup01 INTO ls_popup INDEX p_es_row_no-row_id.
**
**  ls_modi-row_id   = p_es_row_no-row_id.
**  ls_modi-fieldname = 'CHARG'.
**  ls_modi-value     = ls_sel-fieldval.
**  APPEND ls_modi TO <itab>.
**
**  READ TABLE lt_data INTO DATA(ls_data_temp)
**                               WITH KEY charg = ls_sel-fieldval.
**  IF sy-subrc EQ 0.
**    """Memory'den okuyarak depo yeri bilgisi alındı.
**    PERFORM arama_yardimindan_gelen_lgort USING ls_data_temp-lgort.
**    ls_modi-row_id   = p_es_row_no-row_id.
**    ls_modi-fieldname = 'LGORT'.
**    ls_modi-value     = ls_data_temp-lgort.
**    APPEND ls_modi TO <itab>.
**  ENDIF.
**
**  p_er_event_data->m_event_handled = 'X'.
*  ENDMETHOD.

    METHOD handle_double_click.
*      break abemaraba .
      IF e_column EQ 'AWKEY'.
        CLEAR wa_data .
        READ TABLE gt_data INTO wa_data INDEX e_row-index .
        IF sy-subrc IS INITIAL .
          SET PARAMETER ID 'BUK' FIELD wa_data-awkey+10(4) .
          SET PARAMETER ID 'BLN' FIELD wa_data-awkey(10) .
          SET PARAMETER ID 'GJR' FIELD wa_data-awkey+14(4) .
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
        ENDIF .
      ELSEIF e_column EQ 'VIRMAN_AWKEY' .
        CLEAR wa_data .
        READ TABLE gt_data INTO wa_data INDEX e_row-index .
        IF sy-subrc IS INITIAL .
          SET PARAMETER ID 'BUK' FIELD wa_data-virman_awkey+10(4) .
          SET PARAMETER ID 'BLN' FIELD wa_data-virman_awkey(10) .
          SET PARAMETER ID 'GJR' FIELD wa_data-virman_awkey+14(4) .
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
        ENDIF .
      ENDIF .
    ENDMETHOD .

  ENDCLASS .

  DATA : o_event_handler TYPE REF TO lcl_event_handler .

  DATA : t_mwdat LIKE rtax1u15 OCCURS 0 WITH HEADER LINE .
