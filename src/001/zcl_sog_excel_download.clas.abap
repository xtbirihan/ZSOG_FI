class ZCL_SOG_EXCEL_DOWNLOAD definition
  public
  final
  create public .

public section.

  class-methods CREATE_XLS_FROM_ITAB
    importing
      value(IT_FIELDCAT) type LVC_T_FCAT optional
      value(IT_SORT) type LVC_T_SORT optional
      value(IT_FILT) type LVC_T_FILT optional
      value(IS_LAYOUT) type LVC_S_LAYO optional
      value(I_XLSX) type FLAG optional
    exporting
      value(E_XSTRING) type XSTRING
    changing
      value(CT_DATA) type STANDARD TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SOG_EXCEL_DOWNLOAD IMPLEMENTATION.


METHOD create_xls_from_itab.

  DATA: mt_fcat        TYPE lvc_t_fcat.
  DATA: mt_data        TYPE REF TO data.
  DATA: m_flavour      TYPE string.
  DATA: m_version      TYPE string.
  DATA: mo_result_data TYPE REF TO cl_salv_ex_result_data_table.
  DATA: mo_columns     TYPE REF TO cl_salv_columns_table.
  DATA: mo_aggreg      TYPE REF TO cl_salv_aggregations.
  DATA: mo_salv_table  TYPE REF TO cl_salv_table.
  DATA: m_file_type    TYPE salv_bs_constant.
  FIELD-SYMBOLS <tab>  TYPE ANY TABLE.

  GET REFERENCE OF ct_data INTO mt_data.

*if we didn't pass fieldcatalog we need to create it
  IF it_fieldcat[] IS INITIAL.
    ASSIGN mt_data->* TO <tab>.
    TRY .
        cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = mo_salv_table
        CHANGING
          t_table      = <tab> ).
      CATCH cx_salv_msg.

    ENDTRY.
    "get colums & aggregation infor to create fieldcat
    mo_columns  = mo_salv_table->get_columns( ).
    mo_aggreg   = mo_salv_table->get_aggregations( ).
    mt_fcat     =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                  r_columns      = mo_columns
                                  r_aggregations = mo_aggreg ).
  ELSE.
*else we take the one we passed
    mt_fcat[] = it_fieldcat[].
  ENDIF.


  IF cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_25 OR
     cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_26.

    mo_result_data = cl_salv_ex_util=>factory_result_data_table(
        r_data                      = mt_data
        s_layout                    = is_layout
        t_fieldcatalog              = mt_fcat
        t_sort                      = it_sort
        t_filter                    = it_filt
    ).

    CASE cl_salv_bs_a_xml_base=>get_version( ).
      WHEN if_salv_bs_xml=>version_25.
        m_version = if_salv_bs_xml=>version_25.
      WHEN if_salv_bs_xml=>version_26.
        m_version = if_salv_bs_xml=>version_26.
    ENDCASE.

    "if we flag i_XLSX then we'll create XLSX if not then MHTML excel file
    IF i_xlsx IS NOT INITIAL.
      m_file_type = if_salv_bs_xml=>c_type_xlsx.
    ELSE.
      m_file_type = if_salv_bs_xml=>c_type_mhtml.
    ENDIF.


    m_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.
    "transformation of data to excel
    CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
      EXPORTING
        xml_type      = m_file_type
        xml_version   = m_version
        r_result_data = mo_result_data
        xml_flavour   = m_flavour
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = e_xstring.
  ENDIF.


ENDMETHOD.
ENDCLASS.
