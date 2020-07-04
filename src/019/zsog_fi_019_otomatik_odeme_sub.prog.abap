*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_019_OTOMATIK_ODEME_SUB
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*

FORM get_data .
  CLEAR: gt_reguh.
  SELECT
  r~laufd
  r~ubnky
  r~ubknt
  t~iban
  r~name1
  r~rwbtr
  r~waers
  r~zbnkl
  r~zbnky
  r~zbnkn
  r~ziban
  l~stras
  l~ort01
  l~stcd1
  r~lifnr
  r~vblnr
  r~xvorl
  l~telf1
  FROM reguh AS r
  INNER JOIN lfa1 AS l ON l~lifnr = r~lifnr
  LEFT OUTER JOIN tiban AS t ON t~banks = r~ubnks
                            AND t~bankl = r~ubnkl
                            AND t~bankn = r~ubknt
  INTO CORRESPONDING FIELDS OF TABLE gt_reguh
  WHERE zbukr IN s_zbukr
    AND laufi EQ p_laufi
    AND laufd IN s_laufd.

  DELETE gt_reguh WHERE vblnr IS INITIAL.

  IF r1 IS NOT INITIAL.     "gerçek çalış ise
    DELETE gt_reguh WHERE xvorl EQ 'X'.
  ELSEIF r2 IS NOT INITIAL. "öneri çalış ise
    DELETE gt_reguh WHERE xvorl NE 'X'.
  ENDIF.

  SELECT SINGLE adrnr
    INTO lv_adrnr
    FROM t001
    WHERE bukrs IN s_zbukr.

  SELECT SINGLE name1 name3
    FROM adrc
    INTO (lv_name1, lv_name3)
    WHERE addrnumber EQ lv_adrnr.


ENDFORM.                    "get_data
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .

  CLEAR: gt_out,gs_out,sayac,lv_tarih,lv_toplam.
  LOOP AT gt_reguh INTO gs_reguh.

    lv_tarih = gs_reguh-laufd.

    CLEAR: gs_out,lt_string,lv_banka_adi,lv_sube_adi,lv_vergi_no.

    SPLIT gs_reguh-zbnky AT '-' INTO TABLE lt_string.
    MOVE-CORRESPONDING gs_reguh TO gs_out.
    sayac = sayac + 1.
    gs_out-sirno = sayac.

    CLEAR ls_string.
    READ TABLE lt_string INTO ls_string INDEX 1.
    gs_out-zbnkl = ls_string.

    CLEAR ls_string.
    READ TABLE lt_string INTO ls_string INDEX 2.
    gs_out-zbnky = ls_string.

    CLEAR: lv_banka_adi,lv_sube_adi.
    SELECT SINGLE banka brnch
      FROM bnka
      INTO (lv_banka_adi, lv_sube_adi)
      WHERE bankl EQ gs_reguh-zbnkl.

    CLEAR lv_vergi_no.
    SELECT SINGLE stcd2
      FROM lfa1
      INTO lv_vergi_no
      WHERE lifnr EQ gs_reguh-lifnr.

    gs_out-aveno = lv_vergi_no.
    gs_out-abank = lv_banka_adi.
    gs_out-asadi = lv_sube_adi.
    gs_out-istip = 'D'.
    gs_out-acklm = 'Açıklama'.
    IF gs_reguh-rwbtr LT 0.
      gs_out-rwbtr =  -1 * gs_reguh-rwbtr .
    ENDIF.
    lv_toplam = lv_toplam + gs_out-rwbtr.
    APPEND gs_out TO gt_out.
  ENDLOOP.
ENDFORM.                    "modify_data
*&---------------------------------------------------------------------*
*&      Form  SHOW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_data .
  CALL SCREEN 0100.
ENDFORM.                    "show_data
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_alv .
  IF gr_alvgrid IS INITIAL.

    CREATE OBJECT gr_container
      EXPORTING
        container_name              = gc_custom_control_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT gr_alvgrid
      EXPORTING
        i_parent          = gr_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM prepare_fieldcatalog.
    PERFORM change_fieldcat.

    gs_layout-zebra = 'X'.
    gs_layout-sel_mode = 'A'.

    CALL METHOD gr_alvgrid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        it_toolbar_excluding          = gt_toolbar_excluding
        is_variant                    = gs_variant
        i_save                        = 'A'
      CHANGING
        it_outtab                     = gt_out[]
        it_fieldcatalog               = gt_fieldcat[]
        it_sort                       = gt_sort[]
*       it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.

    CALL METHOD gr_alvgrid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDIF.
ENDFORM.                    "show_alv
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_fieldcatalog .
  DATA ls_fieldcat TYPE lvc_s_fcat.

  CLEAR : gt_fieldcat, gt_fieldcat[].
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSOG_FI_019_S_01'
    CHANGING
      ct_fieldcat            = gt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.                    " PREPARE_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_fieldcat .
  LOOP AT gt_fieldcat INTO ls_fieldcat.
    CASE ls_fieldcat-fieldname.
      WHEN 'SRNO'.
        ls_fieldcat-outputlen = 6.
        ls_fieldcat-tech = 'X'.
        ls_fieldcat-no_out = 'X'.
        PERFORM set_fieldtext  USING 'TOPLAM' .
      WHEN 'SIRNO' .
        ls_fieldcat-outputlen = 8.
        PERFORM set_fieldtext   USING 'SIRA NO' .
      WHEN 'LAUFD' .
        ls_fieldcat-outputlen = 12.
        PERFORM set_fieldtext   USING 'İŞLEM TARİHİ' .
      WHEN 'UBNKY' .
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'GÖNDEREN ŞUBE KODU' .
      WHEN 'UBKNT' .
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'GÖNDEREN HESAP NO ' .
      WHEN 'IBAN'.
        ls_fieldcat-outputlen = 28.
        PERFORM set_fieldtext   USING 'GÖNDEREN IBAN' .
      WHEN 'NAME1'.
        ls_fieldcat-outputlen = 10.
        PERFORM set_fieldtext   USING 'ALICI ADI' .
      WHEN 'RWBTR'.
        ls_fieldcat-outputlen = 15.
        ls_fieldcat-do_sum = 'X'.
        PERFORM set_fieldtext   USING 'TUTAR' .
      WHEN 'WAERS'.
        ls_fieldcat-outputlen = 10.
        PERFORM set_fieldtext   USING 'PARA BİRİMİ' .
      WHEN 'ZBNKL'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI BANKA KODU' .
      WHEN 'ABANK'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI BANKA ADI' .
      WHEN 'ZBNKY'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI ŞUBE KODU' .
      WHEN 'ASADI'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI SUBE ADI' .
      WHEN 'ZBNKN'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI HESAP NO' .
      WHEN 'ZIBAN'.
        ls_fieldcat-outputlen = 28.
        PERFORM set_fieldtext   USING 'ALICI IBAN' .
      WHEN 'STRAS'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI ADRES' .
      WHEN 'ORT01'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI ŞEHİR' .
      WHEN 'ACKLM'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'AÇIKLAMA ' .
      WHEN 'GREFR'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'GÖNDEREN REFERANS' .
      WHEN 'AREFR'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI REFERANS' .
      WHEN 'STCD1'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI VERGİ DAİRESİ' .
      WHEN 'AVENO'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'ALICI VERGİ NO' .
      WHEN 'ISTIP'.
        ls_fieldcat-outputlen = 15.
        PERFORM set_fieldtext   USING 'İŞLEM TİPİ ' .
      WHEN 'LIFNR'.
*        ls_fieldcat-no_out = 'X'.
      WHEN 'VBLNR'.
        ls_fieldcat-outputlen = 10.
        PERFORM set_fieldtext   USING 'Ödeme belgesi numarası ' .
    ENDCASE.
    MODIFY gt_fieldcat FROM ls_fieldcat.
  ENDLOOP.

  PERFORM set_sort.
ENDFORM.                    "change_fieldcat
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0182   text
*----------------------------------------------------------------------*
FORM set_fieldtext  USING p_fieldtext TYPE string.
  ls_fieldcat-coltext = p_fieldtext.
  ls_fieldcat-scrtext_s = p_fieldtext.
  ls_fieldcat-scrtext_m = p_fieldtext.
  ls_fieldcat-scrtext_l = p_fieldtext.
ENDFORM.                    "set_fieldtext
*&---------------------------------------------------------------------*
*&      Form  CONVERT_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_excel .

  TYPES: BEGIN OF t_header,
           col01 TYPE string,
           col02 TYPE string,
           col03 TYPE string,
           col04 TYPE string,
           col05 TYPE string,
           col06 TYPE string,
           col07 TYPE string,
           col08 TYPE string,
           col09 TYPE string,
           col10 TYPE string,
           col11 TYPE string,
           col12 TYPE string,
           col13 TYPE string,
           col14 TYPE string,
           col15 TYPE string,
           col16 TYPE string,
           col17 TYPE string,
           col18 TYPE string,
           col19 TYPE string,
           col20 TYPE string,
           col21 TYPE string,
           col22 TYPE string,
         END OF t_header.

  DATA: pv_dest_filepath TYPE string,
        lv_filename      TYPE string,
        lv_path          TYPE string.
  DATA: w_header TYPE t_header,
        i_header TYPE STANDARD TABLE OF t_header.

  w_header-col01 = 'Gönderen Adı Soyadı'.
  w_header-col02 = 'Dosya Tarihi'.
  w_header-col03 = 'Gönderen Vergi No'.
  APPEND w_header TO i_header.
  CLEAR w_header.
  IF lv_tarih IS NOT INITIAL.
    CONCATENATE lv_tarih+6(2) '.' lv_tarih+4(2) '.' lv_tarih+0(4)
    INTO lv_tarih.
  ENDIF.
  w_header-col01 = lv_name1.
  w_header-col02 = lv_tarih.
  w_header-col03 = lv_name3.
  APPEND w_header TO i_header.
  CLEAR w_header.
  w_header-col01 = 'SIRA NO'.
  w_header-col02 = 'İŞLEM TARİHİ'.
  w_header-col03 = 'GÖNDEREN ŞUBE KODU'.
  w_header-col04 = 'GÖNDEREN HESAP NO'.
  w_header-col05 = 'GÖNDEREN IBAN'.
  w_header-col06 = 'ALICI ADI'.
  w_header-col07 = 'TUTAR'.
  w_header-col08 = 'PARA BİRİMİ'.
  w_header-col09 = 'ALICI BANKA KODU'.
  w_header-col10 = 'ALICI BANKA ADI'.
  w_header-col11 = 'ALICI ŞUBE KODU'.
  w_header-col12 = 'ALICI SUBE ADI'.
  w_header-col13 = 'ALICI HESAP NO'.
  w_header-col14 = 'ALICI IBAN'.
  w_header-col15 = 'ALICI ADRES'.
  w_header-col16 = 'ALICI ŞEHİR'.
  w_header-col17 = 'AÇIKLAMA'.
  w_header-col18 = 'GÖNDEREN REFERANS'.
  w_header-col19 = 'ALICI REFERANS'.
  w_header-col20 = 'ALICI VERGİ DAİRESİ'.
  w_header-col21 = 'ALICI VERGİ NO '.
  w_header-col22 = 'İŞLEM TİPİ'.
  APPEND w_header TO i_header.
  CLEAR w_header.
  LOOP AT gt_out INTO gs_out.
    w_header-col01 = gs_out-sirno.
    w_header-col02 = gs_out-laufd.
    w_header-col03 = gs_out-ubnky.
    w_header-col04 = gs_out-ubknt.
    w_header-col05 = gs_out-iban.
    w_header-col06 = gs_out-name1.
    w_header-col07 = gs_out-rwbtr.
    w_header-col08 = gs_out-waers.
    w_header-col09 = gs_out-zbnkl.
    w_header-col10 = gs_out-abank.
    w_header-col11 = gs_out-zbnky.
    w_header-col12 = gs_out-asadi.
    w_header-col13 = gs_out-zbnkn.
    w_header-col14 = gs_out-ziban.
    w_header-col15 = gs_out-stras.
    w_header-col16 = gs_out-ort01.
    w_header-col17 = gs_out-acklm.
    w_header-col18 = gs_out-grefr.
    w_header-col19 = gs_out-arefr.
    w_header-col20 = gs_out-stcd1.
    w_header-col21 = gs_out-aveno.
    w_header-col22 = gs_out-istip.
    APPEND w_header TO i_header.
    CLEAR w_header.
  ENDLOOP.

  cl_gui_frontend_services=>file_save_dialog(
  EXPORTING
    window_title         = 'Select the File Save Location'
    file_filter          = '(*.xls)|*.xls|'
  CHANGING
    filename             = lv_filename
    path                 = lv_path
    fullpath             = pv_dest_filepath
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    OTHERS               = 4
    ).

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      append                  = 'X'
      write_field_separator   = 'X'
      filename                = pv_dest_filepath
      filetype                = 'ASC'
    TABLES
      data_tab                = i_header
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.                               " IF sy-subrc EQ 0
ENDFORM.                    "convert_excel
*&---------------------------------------------------------------------*
*&      Form  WRITE_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_excel .
  DATA: pv_dest_filepath TYPE string,
        lv_filename      TYPE string,
        lv_path          TYPE string.
  IF gt_out[] IS INITIAL.
    MESSAGE 'Veri yok' TYPE 'E' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  cl_gui_frontend_services=>file_save_dialog(
  EXPORTING
    window_title         = 'Farklı Kaydet'
    file_filter          = '(*.xls)|*.xls|'
  CHANGING
    filename             = lv_filename
    path                 = lv_path
    fullpath             = pv_dest_filepath
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    OTHERS               = 4
    ).

  CHECK NOT pv_dest_filepath IS INITIAL.

  IF lv_tarih IS NOT INITIAL.
    CONCATENATE lv_tarih+6(2) '.' lv_tarih+4(2) '.' lv_tarih+0(4)
    INTO lv_tarih.
  ENDIF.

  CREATE OBJECT application 'excel.application'.
  SET PROPERTY OF application 'visible' = 1.
  CALL METHOD OF
      application
      'Workbooks' = workbook.
  CALL METHOD OF
      workbook
      'Add'.

*  CREATE FIRST Excel Sheet
  CALL METHOD OF
      application
      'Worksheets' = sheet
    EXPORTING
      #1           = 1.
  CALL METHOD OF
      sheet
      'Activate'.
  SET PROPERTY OF sheet 'Name' = 'Sheet1'.

  PERFORM fill_cell USING 1 1  'Gönderen Adı Soyadı' 8.
  PERFORM font      USING 1 '12' 0.
  PERFORM fill_cell USING 1 2  'Dosya Tarihi' 6.
  PERFORM font      USING 1 '12' 0.
  PERFORM fill_cell USING 1 3  'Gönderen Vergi No' 9.
  PERFORM font      USING 1 '12' 0.

  PERFORM fill_cell USING 2 1 lv_name1 8.
  PERFORM font      USING 0 '8' 0.
  PERFORM fill_cell USING 2 2 lv_tarih 6.
  PERFORM font      USING 0 '8' 0.
  PERFORM fill_cell USING 2 3 lv_name3 9.
  PERFORM font      USING 0 '8' 0.

  PERFORM fill_cell USING  3 1  'SIRA NO' 8.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 2  'İŞLEM TARİHİ' 6.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 3  'GÖNDEREN ŞUBE KODU' 9.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 4  'GÖNDEREN HESAP NO' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 5  'GÖNDEREN IBAN' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 6  'ALICI ADI' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 7  'TUTAR' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 8  'PARA BİRİMİ' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 9  'ALICI BANKA KODU' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 10 'ALICI BANKA ADI' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 11 'ALICI ŞUBE KODU' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 12 'ALICI SUBE ADI' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 13 'ALICI HESAP NO' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 14 'ALICI IBAN' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 15 'ALICI ADRES' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 16 'ALICI ŞEHİR' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 17 'AÇIKLAMA' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 18 'GÖNDEREN REFERANS' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 19 'ALICI REFERANS' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 20 'ALICI VERGİ DAİRESİ' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 21 'ALICI VERGİ NO ' 10.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  3 22 'İŞLEM TİPİ' 10.
  PERFORM font      USING 1 '10' 0.

*- tablo satırları
  CLEAR v_row.
  LOOP AT gt_out INTO gs_out.
    v_row = sy-tabix + 3.
    PERFORM fill_cell USING  v_row 1  gs_out-sirno 22 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 2  gs_out-laufd 13 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 3  gs_out-ubnky 19 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 4  gs_out-ubknt 19 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 5  gs_out-iban 23 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 6  gs_out-name1 20 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 7  gs_out-rwbtr 8 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 8  gs_out-waers 8 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 9  gs_out-zbnkl 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 10 gs_out-abank 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 11 gs_out-zbnky 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 12 gs_out-asadi 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 13 gs_out-zbnkn 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 14 gs_out-ziban 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 15 gs_out-stras 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 16 gs_out-ort01 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 17 gs_out-acklm 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 18 gs_out-grefr 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 19 gs_out-arefr 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 20 gs_out-stcd1 18 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 21 gs_out-aveno 15 .
    PERFORM font      USING 0 '8' 0.
    PERFORM fill_cell USING  v_row 22 gs_out-istip 10 .
    PERFORM font      USING 0 '8' 0.
  ENDLOOP.

*- toplam satırı
  v_row = v_row + 1.
  PERFORM fill_cell USING  v_row 1 'TOPLAM' 22.
  PERFORM font      USING 1 '10' 0.
  PERFORM fill_cell USING  v_row 7 lv_toplam 8.
  PERFORM font      USING 1 '10' 0.

* Save excel speadsheet to particular filename
  CALL METHOD OF
      sheet
      'SaveAs'

    EXPORTING
      #1       = pv_dest_filepath     "filename
      #2       = 1.                          "fileFormat

*  Closes excel window, data is lost if not saved
  SET PROPERTY OF application 'visible' = 0.
  SET PROPERTY OF application 'DisplayAlerts' = 0.
  CALL METHOD OF
      sheet
      'QUIT'.
  FREE OBJECT application.
  FREE OBJECT sheet.
ENDFORM.                    "write_excel

*&---------------------------------------------------------------------*
*&      Form  fill_cell
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ROW        text
*      -->COL        text
*      -->VAL        text
*      -->SIZ        text
*----------------------------------------------------------------------*
FORM fill_cell  USING row  col val siz.
  CALL METHOD OF
      sheet
      'Cells' = cells
    EXPORTING
      #1      = row
      #2      = col.
  SET PROPERTY OF cells 'Value' = val.
  SET PROPERTY OF cells 'ColumnWidth' = siz.
ENDFORM.                    "fill_cell


*&---------------------------------------------------------------------*
*&      Form  font
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->BOLD       text
*      -->SIZE       text
*      -->ITALIC     text
*----------------------------------------------------------------------*
FORM font USING bold size italic.
  CALL METHOD OF
      cells
      'FONT' = font.
  SET PROPERTY OF font 'BOLD'   = bold.
  SET PROPERTY OF font 'SIZE'   = size.
  SET PROPERTY OF font 'ITALIC' = italic.
*
*  CALL METHOD OF lo_range 'select'.
*  SET PROPERTY OF lo_range 'HorizontalAlignment' = 2.
*  FREE OBJECT font.
ENDFORM.                    "font
*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_sort .
  CLEAR: gt_sort,gt_sort[].
*  gt_sort-spos = 1.
  gt_sort-fieldname = 'SRNO'.
*  gt_sort-up = 'X'.
  gt_sort-subtot = 'X'.

  APPEND gt_sort.
  CLEAR gt_sort.
ENDFORM.                    "set_sort
*&---------------------------------------------------------------------*
*&      Form  F_GET_LAUFI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_laufi .
  DATA: BEGIN OF lt_laufi OCCURS 0,
          laufi TYPE reguh-laufi,
        END OF lt_laufi.
  DATA: return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE.

  CLEAR: lt_laufi,lt_laufi[].
  SELECT DISTINCT laufi FROM reguh INTO TABLE lt_laufi.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'LAUFI'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_LAUFI'
      value_org   = 'S'
    TABLES
      value_tab   = lt_laufi
      return_tab  = return.
ENDFORM.                    "f_get_laufi
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_TXT
*&---------------------------------------------------------------------*

FORM download_txt .

  DATA : filename TYPE string.
  DATA : lv_baslık TYPE string.
  DATA : lv_data TYPE string.
  DATA : lv_data1 TYPE string.
  DATA : lv_data2 TYPE string.
  DATA : lv_data3 TYPE string.
  DATA : lv_data4 TYPE string.
  DATA : lv_data5 TYPE string.
  DATA : lv_data6 TYPE string.
  DATA : lv_data7 TYPE string.
  DATA : lv_data8 TYPE string.
  DATA : lv_data9 TYPE string.
  DATA : lv_data10 TYPE string.


  DATA:lv_add      TYPE int1,
       lv_aciklama TYPE string,
       lv_zbnkn    TYPE string,
       lv_name1    TYPE string,
       lv_stras    TYPE string,
       lv_telf1    TYPE string,
       lv_acikla   TYPE string,
       lv_tutar    TYPE string,
       lv_tutar1    TYPE string,
       lv_arefr    TYPE string,
       lv_waers    TYPE string,
       lv_ubnky    TYPE string.

  DATA: ls_txt LIKE LINE OF gt_txt.

***  PERFORM get_filename USING filename.
  DATA: pv_dest_filepath TYPE string,
        lv_filename      TYPE string,
        lv_path          TYPE string.
  IF gt_out[] IS INITIAL.
    MESSAGE 'Veri yok' TYPE 'E' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  cl_gui_frontend_services=>file_save_dialog(
  EXPORTING
    window_title         = 'Farklı Kaydet'
    file_filter          = '(*.txt)|*.txt|'
  CHANGING
    filename             = filename
    path                 = lv_path
    fullpath             = pv_dest_filepath
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    OTHERS               = 4
    ).

  filename = pv_dest_filepath.

  CONCATENATE 'B123456789' gs_out-laufd INTO lv_baslık SEPARATED BY '          '.
  ls_txt = lv_baslık.
  APPEND ls_txt TO gt_txt.

  LOOP AT gt_out INTO gs_out.
    IF strlen( gs_out-zbnkn ) < 18.
      lv_add = 18 - strlen( gs_out-zbnkn ) .
      lv_zbnkn = gs_out-zbnkn.
      DO lv_add TIMES.
        lv_zbnkn =  lv_zbnkn && | | .
      ENDDO.
    ENDIF.
    CONCATENATE gs_out-ubnky+0(4) gs_out-ubnky+5(5) INTO lv_ubnky.
    CONCATENATE 'D' lv_ubnky gs_out-ubknt+9(7) INTO lv_data.
    CONCATENATE gs_out-zbnkl gs_out-zbnky lv_zbnkn  INTO lv_data1.
    CONCATENATE lv_data lv_data1 INTO lv_data2
    SEPARATED BY '                                  '.

    IF strlen( gs_out-name1 ) < 40.
      lv_add = 40 - strlen( gs_out-name1 ) .
      lv_name1 = gs_out-name1.
      DO lv_add TIMES.
        lv_name1 =  lv_name1 && | | .
      ENDDO.
    ENDIF.

    IF strlen( gs_out-stras ) < 40.
      lv_add = 40 - strlen( gs_out-stras ) .
      lv_stras = gs_out-stras.
      DO lv_add TIMES.
        lv_stras =  lv_stras && | | .
      ENDDO.
    ENDIF.

    IF strlen( gs_out-telf1 ) < 20.
      lv_add = 20 - strlen( gs_out-telf1 ) .
      lv_telf1 = gs_out-telf1.
      DO lv_add TIMES.
        lv_telf1 =  lv_telf1 && | | .
      ENDDO.
    ENDIF.
    CONCATENATE lv_data2 gs_out-lifnr lv_name1 lv_stras lv_telf1 INTO lv_data3.

    lv_aciklama = '//F110 Otomatik Ödeme//'.
    IF strlen( lv_aciklama ) < 40.
      lv_add = 40 - strlen( lv_aciklama ) .
      lv_acikla = lv_aciklama.
      DO lv_add TIMES.
        lv_acikla =  lv_acikla && | | .
      ENDDO.
    ENDIF.
    CONCATENATE '//' gs_out-lifnr '//' '-' lv_acikla  INTO lv_data4.
    CONCATENATE lv_data3 lv_data4 INTO lv_data5
    SEPARATED BY '                              '.

*    IF strlen( gs_out-arefr ) < 17.
*      lv_add = 16 - strlen( gs_out-arefr ) .
*      lv_arefr = gs_out-arefr.
*      DO lv_add TIMES.
*        lv_arefr =  lv_arefr && | | .
*      ENDDO.
*    ENDIF.
*    lv_arefr
*    CONCATENATE lv_data5 lv_arefr INTO lv_data6.

    lv_tutar = gs_out-rwbtr.
    REPLACE  '.' IN lv_tutar WITH ','.

    IF strlen( gs_out-waers ) < 5.
      lv_add = 5 - strlen( gs_out-waers ) .
      lv_waers = gs_out-waers.
      DO lv_add TIMES.
        lv_waers =  lv_waers && | | .
      ENDDO.
    ENDIF.

    IF strlen( lv_tutar ) < 19.
      lv_add = 19 - strlen( lv_tutar ) .
      lv_tutar1 = lv_tutar.
      DO lv_add TIMES.
        lv_tutar1 = '0' && lv_tutar1.
      ENDDO.
    ENDIF.
*    CONCATENATE lv_tutar1 lv_waers INTO lv_data10.
    CONDENSE lv_tutar1 NO-GAPS.
    CONCATENATE lv_tutar1 lv_waers gs_out-laufd '0000' INTO lv_data7."""""""
    CONCATENATE lv_data5 lv_data7 INTO lv_data8
    SEPARATED BY '                                         '.

    CONCATENATE lv_data8 gs_out-aveno INTO lv_data10
    SEPARATED BY '          '.
*    CONCATENATE lv_data7 lv_waers gs_out-laufd '0000' gs_out-aveno INTO lv_data8.
    CONCATENATE lv_data10  gs_out-ziban INTO lv_data9
*    CONCATENATE lv_data8 gs_out-ziban INTO lv_data9
    SEPARATED BY '                                                   '.
    ls_txt = lv_data9.
    APPEND ls_txt TO gt_txt.
  ENDLOOP.



*  CONCATENATE sy-datum+6(2)
*              sy-datum+4(2)
*              sy-datum+0(4)
*        INTO lv_date SEPARATED BY '-' .
*  CONCATENATE sy-uzeit+0(2)
*              sy-uzeit+2(2)
*              sy-uzeit+4(2)
*        INTO lv_time SEPARATED BY '-' .

*  lv_name = |ABCHOUSE_| && lv_date && |-| && lv_time.


  CHECK filename IS NOT INITIAL.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE            =
      filename                = filename
      filetype                = 'ASC'
*     APPEND                  = ' '
*     WRITE_FIELD_SEPARATOR   = ' '
*     HEADER                  = '00'
*     TRUNC_TRAILING_BLANKS   = ' '
*     WRITE_LF                = 'X'
*     COL_SELECT              = ' '
*     COL_SELECT_MASK         = ' '
*     DAT_MODE                = ' '
*     CONFIRM_OVERWRITE       = ' '
*     NO_AUTH_CHECK           = ' '
      codepage                = '4110'
*     IGNORE_CERR             = ABAP_TRUE
*     REPLACEMENT             = '#'
*     WRITE_BOM               = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT            = ' '
*     WK1_N_SIZE              = ' '
*     WK1_T_FORMAT            = ' '
*     WK1_T_SIZE              = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS    = ABAP_TRUE
*     VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*     FILELENGTH              =
    TABLES
      data_tab                = gt_txt[] "gt_txt
*     FIELDNAMES              =
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    "download_txt

*&---------------------------------------------------------------------*
*&      Form  get_filename
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME text
*----------------------------------------------------------------------*
FORM get_filename  USING    p_filename.
  DATA : filetab TYPE filetable,
         rc      TYPE i.
  DATA : ls_filetab LIKE LINE OF filetab.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = 'Select Source Excel File'
      default_extension = '*'
*     FILE_FILTER       = '(*.xls)|*.xls|(*.xlsl|*.xlsl|'
    CHANGING
      file_table        = filetab
      rc                = rc.

  READ TABLE filetab INTO ls_filetab INDEX 1.
  p_filename = ls_filetab-filename.
ENDFORM.                    "get_filename
