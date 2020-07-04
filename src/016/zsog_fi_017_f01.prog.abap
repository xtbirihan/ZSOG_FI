*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_017_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F4_OPEN_FILE
*&---------------------------------------------------------------------*

FORM f4_open_file .

  DATA: lt_filetable    TYPE filetable,  "type table
        ls_filetable    TYPE file_table, "structure
        lv_return_code  TYPE i,
        lv_window_title TYPE string.

  lv_window_title = text-001.
  ""dosya seçim ekranını açar
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_window_title ""pencere başlığı
      default_extension       = c_ext_xls ""varsayılan uzantı
      file_filter             = '(*.XLSX)|*.XLSX|' "" filtre
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_return_code
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  READ TABLE lt_filetable INTO ls_filetable INDEX 1.
  p_file = ls_filetable-filename.
ENDFORM.                    " F4_OPEN_FILE
*&---------------------------------------------------------------------*
*&      Form  EXCEL_UPLOAD
*&---------------------------------------------------------------------*

FORM excel_upload .
  TYPES: BEGIN OF ltt_kna1,
      kunnr TYPE kna1-kunnr,
      anred TYPE kna1-anred,
      name1 TYPE kna1-name1,
      ort01 TYPE kna1-ort01,
      ort02 TYPE kna1-ort02,
      stras TYPE kna1-stras,
      stcd1 TYPE kna1-stcd1,
      stcd2 TYPE kna1-stcd2,
      katr2 TYPE kna1-katr2,
      katr6 TYPE kna1-katr6,
      deposit_amount TYPE zsog_fi_001_t_01-deposit_amount,
      loevm TYPE kna1-loevm,
      zguncel_limit TYPE zsog_fi_012_t_02-zguncel_limit,
    END OF ltt_kna1.


  DATA: lt_kna1 TYPE TABLE OF ltt_kna1,
        ls_kna1 TYPE ltt_kna1.

  CLEAR gt_file[].
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = '1'    "başlangıç sütunu
      i_begin_row             = '2'    "başlangıc satırı
      i_end_col               = '3'   "bitiş sütunu
      i_end_row               = '100000'  "bitiş satırı ( max satır)
    TABLES
      intern                  = gt_file
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc = 0.
    "" LOOP içerisinde satır satır döner.
    LOOP AT gt_file INTO gs_file.
      CASE gs_file-col.
        WHEN '0001'.
          MOVE gs_file-value TO gs_out-kunnr.
        WHEN '0002'.
          MOVE gs_file-value TO gs_out-zyuklenen_limit.
      ENDCASE.
      AT END OF row.
        APPEND gs_out TO gt_out.
        CLEAR: gs_out, gs_file.
      ENDAT.
    ENDLOOP.

    SELECT k~kunnr k~anred k~name1 k~ort01 k~ort02 k~stras k~stcd1
           k~stcd2 k~katr2 k~katr6 z01~deposit_amount k~loevm
           z02~zguncel_limit
      FROM kna1 AS k
 LEFT JOIN zsog_fi_001_t_01 AS z01
        ON z01~retail_location_id = k~kunnr
  LEFT JOIN zsog_fi_012_t_02 AS z02
        ON z02~kunnr = k~kunnr
INTO TABLE lt_kna1
      FOR ALL ENTRIES IN gt_out
        WHERE k~kunnr = gt_out-kunnr.


    SORT: gt_out BY kunnr.
    SORT: lt_kna1 BY kunnr.

    LOOP AT gt_out INTO gs_out.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = gs_out-kunnr
      BINARY SEARCH.
      IF sy-subrc = 0.
        gs_out-anred = ls_kna1-anred.
        gs_out-name1 = ls_kna1-name1.
        gs_out-ort01 = ls_kna1-ort01.
        gs_out-ort02 = ls_kna1-ort02.
        gs_out-stras = ls_kna1-stras.
        gs_out-stcd1 = ls_kna1-stcd1.
        gs_out-stcd2 = ls_kna1-stcd2.
        gs_out-katr2 = ls_kna1-katr2.
        gs_out-katr6 = ls_kna1-katr6.
        gs_out-deposit_amount = ls_kna1-deposit_amount.
        gs_out-loevm = ls_kna1-loevm.
        gs_out-zguncel_limit = ls_kna1-zguncel_limit.
        MODIFY gt_out FROM gs_out.
      ENDIF.
      CLEAR: gs_out, ls_kna1.
    ENDLOOP.

*DELETE ADJACENT DUPLICATES FROM gt_out COMPARING ALL FIELDS.
  ENDIF.
  IF gt_out IS INITIAL.
    gv_error = 'X'.
    MESSAGE s125(zmm) WITH 'Dosya okunamadı' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " EXCEL_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fieldcatalog .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = 'ZFI_S_VA7'
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = gt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CLEAR gs_fieldcat.
  LOOP AT gt_fieldcat INTO gs_fieldcat.
    CASE gs_fieldcat-fieldname.
      WHEN 'KUNNR'.
        gs_fieldcat-scrtext_s = 'Bayi No'.
        gs_fieldcat-scrtext_m = 'Bayi No'.
        gs_fieldcat-scrtext_l = 'Bayi No'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-hotspot = 'X'.
        gs_fieldcat-col_pos   = 1.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'ANRED'.
        gs_fieldcat-scrtext_s = 'Şirket Tipi'.
        gs_fieldcat-scrtext_m = 'Şirket Tipi'.
        gs_fieldcat-scrtext_l = 'Şirket Tipi'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 2.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'NAME1'.
        gs_fieldcat-scrtext_s  = 'Bayi Adı'.
        gs_fieldcat-scrtext_m  = 'Bayi Adı'.
        gs_fieldcat-scrtext_l  = 'Bayi Adı'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 3.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'ORT01'.
        gs_fieldcat-scrtext_s = 'Şehir'.
        gs_fieldcat-scrtext_m = 'Şehir'.
        gs_fieldcat-scrtext_l = 'Şehir'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 4.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'STRAS'.
*        gs_fieldcat-scrtext_s = 'Adres'.
*        gs_fieldcat-scrtext_m = 'Adres'.
*        gs_fieldcat-scrtext_l = 'Adres'.
*        gs_fieldcat-colddictxt = 'M'.
*        gs_fieldcat-col_pos   = 6.
        gs_fieldcat-no_out     = 'X'.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'STCD1'.
        gs_fieldcat-scrtext_s = 'Vergi D'.
        gs_fieldcat-scrtext_m = 'Vergi Dairesi'.
        gs_fieldcat-scrtext_l = 'Vergi Dairesi'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 7.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'STCD2'.
        gs_fieldcat-scrtext_s = 'Vergi No'.
        gs_fieldcat-scrtext_m = 'Vergi No'.
        gs_fieldcat-scrtext_l = 'Vergi No'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 8.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'KATR2'.
        gs_fieldcat-colddictxt = 'L'.
        gs_fieldcat-col_pos   = 6.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'KATR6'.
*        gs_fieldcat-scrtext_s = ''.
        gs_fieldcat-scrtext_m = 'Şans Bayi Mi'.
        gs_fieldcat-scrtext_l = 'Şans Bayi Mi'.
        gs_fieldcat-colddictxt = 'L'.
        gs_fieldcat-col_pos   = 10.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'DEPOSIT_AMOUNT'.
        gs_fieldcat-scrtext_s = 'Hasılat Teminatı'.
        gs_fieldcat-scrtext_m = 'Hasılat Teminatı'.
        gs_fieldcat-scrtext_l = 'Hasılat Teminatı'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 11.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'ZGUNCEL_LIMIT'.
        gs_fieldcat-scrtext_s = 'S.Yklnn Lmt'.
        gs_fieldcat-scrtext_m = 'Son Yüklenen Limit'.
        gs_fieldcat-scrtext_l = 'Son Yüklenen Limit'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 12.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'ZYUKLENEN_LIMIT'.
        gs_fieldcat-scrtext_s = 'YUK LMT'.
        gs_fieldcat-scrtext_m = 'Yüklenen Limit'.
        gs_fieldcat-scrtext_l = 'Yüklenen Limit'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 13.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'LOEVM'.
        gs_fieldcat-no_out     = 'X'.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'MESSAGE'.
        gs_fieldcat-scrtext_s = 'Mesaj'.
        gs_fieldcat-scrtext_m = 'Mesaj Metni'.
        gs_fieldcat-scrtext_l = 'Mesaj Metni'.
        gs_fieldcat-colddictxt = 'M'.
        gs_fieldcat-col_pos   = 14.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'DURUM'.
        gs_fieldcat-no_out     = 'X'.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      WHEN 'KONTROL'.
        gs_fieldcat-no_out     = 'X'.
        MODIFY gt_fieldcat FROM gs_fieldcat.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    "FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  show_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_data .
  DATA: lcl_alv_event TYPE REF TO lcl_event_receiver.
  CLEAR: gt_fieldcat, gt_fieldcat[].

  PERFORM fieldcatalog.

  IF gr_container IS INITIAL.

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

    gs_layout-zebra = 'X'.
    gs_layout-cwidth_opt = 'X'.
    gs_layout-info_fname = 'COLOR'.
    gs_layout-sel_mode = 'A'.

    CREATE OBJECT lcl_alv_event.
    CREATE OBJECT go_eventreceiver2.


    SET HANDLER lcl_alv_event->hotspot_click           FOR gr_alvgrid.
    SET HANDLER lcl_alv_event->handle_data_changed     FOR gr_alvgrid.
    SET HANDLER go_eventreceiver2->handle_double_click FOR gr_alvgrid.

    CALL METHOD gr_alvgrid->set_table_for_first_display
      EXPORTING
*       I_BUFFER_ACTIVE               =
*       I_BYPASSING_BUFFER            =
*       I_CONSISTENCY_CHECK           =
*       I_STRUCTURE_NAME              =
*       IS_VARIANT                    =
*       I_SAVE                        =
*       I_DEFAULT                     = 'X'
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_out[]
        it_fieldcatalog               = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD gr_alvgrid->refresh_table_display.
  ENDIF.

ENDFORM.                    " SHOW_DATA
*&---------------------------------------------------------------------*
*&      Form  excel_download_sample
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excel_download_sample .

  REFRESH : it_tab1.

  CREATE OBJECT w_excel 'EXCEL.APPLICATION'.
  SET PROPERTY OF w_excel  'VISIBLE' = 1.

  CALL METHOD OF
      w_excel
      'WORKBOOKS' = w_workbook.

  CALL METHOD OF
      w_workbook
      'ADD'.

  SET PROPERTY OF w_excel 'SheetsInNewWorkbook' = 3.

  ASSIGN w_deli TO <fs> TYPE 'X'.
  w_hex = wl_c09.
  <fs> = w_hex.

  CONCATENATE 'Bayi Numarası'
              'Yuklenen Limit'
              INTO wa_tab SEPARATED BY w_deli.
  APPEND wa_tab TO it_tab1.

* Downloading header details to first sheet
  PERFORM download_sheet TABLES it_tab1 USING 1 'Header Details'.

  GET PROPERTY OF w_excel 'ActiveSheet' = w_worksheet.

ENDFORM.                    "excel_download_sample

*&---------------------------------------------------------------------*
*&      Form  download_sheet
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TAB      text
*      -->P_SHEET    text
*      -->P_NAME     text
*----------------------------------------------------------------------*
FORM download_sheet TABLES p_tab
                     USING p_sheet TYPE i
                           p_name TYPE string.

  CALL METHOD OF
      w_excel
      'WORKSHEETS' = w_worksheet
    EXPORTING
      #1           = p_sheet.

  CALL METHOD OF
      w_worksheet
      'ACTIVATE'.
  SET PROPERTY OF w_worksheet 'NAME' = p_name.

  CALL METHOD OF
      w_excel
      'Range' = w_range
    EXPORTING
      #1      = 'A1'
      #2      = 'AF1'.

  CALL METHOD OF
      w_range
      'INTERIOR' = w_int.
*  SET PROPERTY OF w_int 'ColorIndex' = 6.
  SET PROPERTY OF w_int 'Pattern' = 1.

* Initially unlock all the columns( by default all the columns are
*  LOCKED )
  CALL METHOD OF
      w_excel
      'Columns' = w_columns.
  SET PROPERTY OF w_columns 'Locked' = 0.

* Locking and formatting first column
  CALL METHOD OF
      w_excel
      'Columns' = w_columns
    EXPORTING
      #1        = 1.

  SET PROPERTY OF w_columns  'Locked' = 1.
  SET PROPERTY OF w_columns  'NumberFormat' = '@'.

* Export the contents in the internal table to the clipboard
  CALL METHOD cl_gui_frontend_services=>clipboard_export
    IMPORTING
      data                 = p_tab[]
    CHANGING
      rc                   = w_rc
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

* Paste the contents in the clipboard to the worksheet
  CALL METHOD OF
      w_worksheet
      'Paste'.

* Autofit the columns according to the contents
  CALL METHOD OF
      w_excel
      'Columns' = w_columns.
  CALL METHOD OF
      w_columns
      'AutoFit'.

  FREE OBJECT: w_columns, w_range.

ENDFORM.                    "download_sheet


*&---------------------------------------------------------------------*
*&      Form  EXCEL_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excel_control .
  DATA lv_extention(10) TYPE c.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      IF p_file IS INITIAL.
        MESSAGE e121(zmm) WITH 'Lütfen dosya yolu giriniz.'
        DISPLAY LIKE 'E'.
      ENDIF.
      CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
        EXPORTING
          filename  = p_file
        IMPORTING
          extension = lv_extention.
      IF lv_extention = 'XLS' OR lv_extention = 'XLSX'.
      ELSE.
        MESSAGE e122(zmm) WITH 'XLS uzantılı bir dosya yükleyiniz'
        DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
ENDFORM.                    " EXCEL_CONTROL
*&---------------------------------------------------------------------*
*&      Form  CONTROLS
*&---------------------------------------------------------------------*

FORM controls .

  DATA: lv_count TYPE i.
  DATA: lv_hata TYPE string.
  DATA: ls_out2 TYPE zfi_s_va7.

  DATA: BEGIN OF ls_kunnr,
       kunnr LIKE knb1-kunnr,
       bukrs LIKE knb1-bukrs,
    END OF ls_kunnr,
    lt_kunnr LIKE TABLE OF ls_kunnr.

  SELECT kunnr
    FROM kna1
    INTO TABLE lt_kunnr
    FOR ALL ENTRIES IN gt_out
    WHERE kunnr = gt_out-kunnr
      AND spras EQ sy-langu.

  SORT lt_kunnr BY kunnr.

  LOOP AT gt_out INTO gs_out.
    CLEAR: lv_count, lv_hata, gt_hatalar.

    IF gt_hatalar IS NOT INITIAL.
      LOOP AT gt_hatalar INTO gs_hatalar.
        CONCATENATE lv_hata  gs_hatalar INTO lv_hata  SEPARATED BY ' /'.
      ENDLOOP.
      gs_out-message = lv_hata.
      gs_out-color = 'C310'.
    ENDIF.
    MODIFY gt_out FROM gs_out TRANSPORTING message color.
*    CLEAR: lv_count, lv_hata, gt_hatalar.

    IF ls_out2-kunnr = gs_out-kunnr.
        gs_hatalar-hata = 'Dublicate kayıt giremezsiniz!'.
      APPEND gs_hatalar TO gt_hatalar.
      gs_out-kontrol = 'X'.
      MODIFY gt_out FROM gs_out TRANSPORTING kontrol.
    ENDIF.

* Bayi No alan kontrolü
    IF gs_out-kunnr IS INITIAL.
      gs_hatalar-hata = 'Bayi No alanı boş girilemez!'.
      APPEND gs_hatalar TO gt_hatalar.
      gs_out-kontrol = 'X'.
      MODIFY gt_out FROM gs_out TRANSPORTING kontrol.
    ELSE.
      READ TABLE lt_kunnr TRANSPORTING NO FIELDS
      WITH KEY kunnr = gs_out-kunnr BINARY SEARCH.
      IF sy-subrc NE 0.
        gs_hatalar-hata = 'Bayi No geçersiz!'.
        APPEND gs_hatalar TO gt_hatalar.
        gs_out-kontrol = 'X'.
        MODIFY gt_out FROM gs_out TRANSPORTING kontrol.
      ENDIF.
    ENDIF.

    IF gs_out-zyuklenen_limit EQ '0.00'."IS INITIAL.
      gs_hatalar-hata = 'Yüklenen limit alanı boş girilemez!'.
      APPEND gs_hatalar TO gt_hatalar.
      gs_out-kontrol = 'X'.
      MODIFY gt_out FROM gs_out TRANSPORTING kontrol.
    ENDIF.

    IF gs_out-katr2 NE '4'.
      gs_hatalar-hata = 'Bayi durumu 4 olmalıdır'.
      APPEND gs_hatalar TO gt_hatalar.
      gs_out-kontrol = 'X'.
      MODIFY gt_out FROM gs_out TRANSPORTING kontrol.
    ENDIF.

    IF gs_out-katr6 EQ 'N'.
      gs_hatalar-hata = 'Şanslı bayi değil'.
      APPEND gs_hatalar TO gt_hatalar.
      gs_out-kontrol = 'X'.
      MODIFY gt_out FROM gs_out TRANSPORTING kontrol.
    ENDIF.

    IF gs_out-loevm EQ 'X'.
      gs_hatalar-hata = 'Bayi silmek üzere işaretlendi'.
      APPEND gs_hatalar TO gt_hatalar.
      gs_out-kontrol = 'X'.
      MODIFY gt_out FROM gs_out TRANSPORTING kontrol.
    ENDIF.

* hata mesajları
    IF gt_hatalar IS NOT INITIAL.
      LOOP AT gt_hatalar INTO gs_hatalar.
        CONCATENATE lv_hata  gs_hatalar INTO lv_hata  SEPARATED BY ' /'.
      ENDLOOP.
      CONCATENATE  'Detayları görmek için çift tıklayınız ' lv_hata
  INTO lv_hata SEPARATED BY ' - '.
      gs_out-message = lv_hata.
      gs_out-color = 'C600'.
    ENDIF.

    MODIFY gt_out FROM gs_out TRANSPORTING message color.
    MOVE gs_out-kunnr to ls_out2-kunnr.
    CLEAR: lv_count, lv_hata, gt_hatalar, gs_out.
  ENDLOOP.
ENDFORM.                    " CONTROLS

*&---------------------------------------------------------------------*
*&      Form  F_HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM f_handle_data_changed
  CHANGING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_good             TYPE lvc_s_modi, "C
**          lv_value_s_belgesi  LIKE gt_out-s_belgesi,
           lv_value_color_line TYPE char4,
           lv_value_durum      TYPE c,
           lv_error            TYPE c. "C.
**    "C
  LOOP AT er_data_changed->mt_mod_cells INTO ls_good.
    CLEAR lv_error.
*
    CASE ls_good-fieldname.
      WHEN 'COLOR'.
        CLEAR gt_out.
        READ TABLE gt_out INTO gs_out INDEX ls_good-row_id.
        IF sy-subrc = 0.
          "C
          CLEAR lv_value_color_line.
          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = lv_value_color_line.


          gs_out-color = lv_value_color_line .
*            """ alv giriş kontrol
          IF gs_out-color  IS NOT INITIAL.
            MODIFY gt_out FROM gs_out INDEX ls_good-row_id
                                      TRANSPORTING color .
          ENDIF.
          IF lv_error = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  IF lv_error IS NOT INITIAL.
    CALL METHOD er_data_changed->display_protocol.
  ENDIF.

ENDFORM.                    "f_handle_data_changed

*&---------------------------------------------------------------------*
*&      Form  LOG_KAYDET
*&---------------------------------------------------------------------*
FORM log_kaydet .
  DATA: lt_log TYPE STANDARD TABLE OF zsog_fi_012_t_01,
        ls_log TYPE zsog_fi_012_t_01.

  READ TABLE gt_out WITH KEY color = 'C600' TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    LOOP AT gt_out INTO gs_out.
      ls_log-mandt = sy-mandt.
      ls_log-kunnr = gs_out-kunnr.
      ls_log-datum = sy-datum.
      ls_log-time = sy-uzeit.
      ls_log-zyuklenen_limit = gs_out-zyuklenen_limit.
      ls_log-uname = sy-uname.
      APPEND ls_log TO lt_log .
      CLEAR: ls_log.
    ENDLOOP.
  ENDIF.

  MODIFY zsog_fi_012_t_01 FROM TABLE lt_log .
  IF lt_log IS NOT INITIAL.
    COMMIT WORK AND WAIT .

    MESSAGE 'Kayıtlar başarıyla kaydedilmiştir.'
        TYPE 'S'.
    EXIT.
  ELSE.
    ROLLBACK WORK.
    MESSAGE
    'Kayıtların güncellenmesi sırasında hata oluştu.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
ENDFORM.                    " LOG_KAYDET

*&---------------------------------------------------------------------*
*&      Form  KAYDET
*&---------------------------------------------------------------------*

FORM kaydet .
  DATA: lt_log1 TYPE STANDARD TABLE OF zsog_fi_012_t_02,
        ls_log1 TYPE zsog_fi_012_t_02.

  READ TABLE gt_out WITH KEY color = 'C600' TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    LOOP AT gt_out INTO gs_out.
      ls_log1-mandt = sy-mandt.
      ls_log1-kunnr = gs_out-kunnr.
      ls_log1-zguncel_limit = gs_out-zyuklenen_limit.
      ls_log1-uname = sy-uname.
      APPEND ls_log1 TO lt_log1 .
      CLEAR: ls_log1.
    ENDLOOP.
  ENDIF.

  MODIFY zsog_fi_012_t_02 FROM TABLE lt_log1 .

  IF lt_log1 IS NOT INITIAL.
    COMMIT WORK AND WAIT .

    MESSAGE 'Kayıtlar başarıyla güncellenmiştir.'
        TYPE 'S'.
    EXIT.
  ELSE.
    ROLLBACK WORK.
    MESSAGE
    'Kayıtların güncellenmesi sırasında hata oluştu.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.                    " KAYDET

*&---------------------------------------------------------------------*
*&      Form  get_onay
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT     text
*----------------------------------------------------------------------*
FORM get_onay  USING p_text TYPE itex132.

  CLEAR:gv_onay.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = p_text
      text_button_1         = 'Evet'
      text_button_2         = 'Hayır'
      default_button        = '1'
      display_cancel_button = 'X'
    IMPORTING
      answer                = gv_onay.

ENDFORM .                    "get_onay
