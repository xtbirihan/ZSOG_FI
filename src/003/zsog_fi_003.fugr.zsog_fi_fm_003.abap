FUNCTION zsog_fi_fm_003.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_RETAILER_NO) TYPE  KUNNR OPTIONAL
*"     VALUE(I_CREATE_DATE) TYPE  ZCREATE_DATE OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETAILER_INFO) TYPE  ZFI_TT_003
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  DATA : lt_return        TYPE TABLE OF bapiret2,
         ls_return        TYPE bapiret2.
  DATA : lv_error         TYPE c,
         lv_dummy         TYPE c.
  DATA : lv_erdat         TYPE kna1-erdat.
  DATA : lr_retailer_no   TYPE RANGE OF knb1-kunnr WITH HEADER LINE,
         lr_create_date   TYPE RANGE OF kna1-erdat WITH HEADER LINE.
*{   ->>> Commented by Prodea Ozan Şahin - 19.11.2019 11:16:19
*  CONSTANTS lv_objectclas TYPE cdhdr-objectclas VALUE 'DEBI'.
*  CONSTANTS lv_tcode      TYPE cdhdr-tcode      VALUE 'XD02'.
*  CONSTANTS lv_username   TYPE cdhdr-username   VALUE 'SGSADMIN'.
*}     <<<- End of  Commented - 19.11.2019 11:16:19
*{   ->>> Added by Prodea Ozan Şahin - 19.11.2019 11:51:42
  DATA: lv_kunnr TYPE kna1-kunnr VALUE '2425'.
  DATA: lv_sg(3) TYPE c          VALUE 'SG%'.
*}     <<<- End of  Added - 19.11.2019 11:51:42

  IF i_retailer_no IS NOT INITIAL.
    lr_retailer_no = 'IEQ'.
    lr_retailer_no-low = i_retailer_no.
    COLLECT lr_retailer_no.
  ENDIF.

  IF i_create_date IS NOT INITIAL.
    lr_create_date = 'IEQ'.
    CONCATENATE i_create_date+4(4)
    i_create_date+2(2) i_create_date+0(2)
    INTO i_create_date.
    lr_create_date-low = i_create_date.
    COLLECT lr_create_date.
  ENDIF.

*{   ->>> Commented by Prodea Ozan Şahin - 19.11.2019 11:08:20
*  SELECT COUNT(*)
*    FROM kna1
*    INTO lv_erdat
*    WHERE erdat EQ lr_create_date-low.
*  IF sy-subrc NE 0.
*    MESSAGE e252(zfi) INTO lv_dummy.
*    PERFORM get_message_text CHANGING ls_return-message.
*    ls_return-type   = 'E'.
*    ls_return-id     = 'ZFI'.
*    ls_return-number = '252'.
*    APPEND ls_return TO et_return[].
*  ENDIF.
*}     <<<- End of  Commented - 19.11.2019 11:08:20

  IF et_return IS INITIAL.

*{   ->>> Commented by Prodea Ozan Şahin - 19.11.2019 11:17:28

*    SELECT z1~retail_location_id
*           z1~retailer_sub_object
*           z1~action_code
*           z1~business_name
*           z1~contact_name
*           z1~type
*           z1~classification
*           z1~street
*           z1~house_number
*           z1~zip_code
*           k~regio
*           z1~sales_district
*           z1~state
*           z1~delivery_street
*           z1~delivery_house_number
*           z1~delivery_zip_code
*           z1~delivery_city
*           z3~districts
*           z1~delivery_state
*           z1~phone1
*           z1~phone2
*           z1~mobile_phone
*           z1~email
*           z1~fax
*           z1~turkish_national_id
*           z1~tax_house
*           z2~tax_number
*           z1~supervisor
*           z1~sales_territory_region
*           z1~commission_package_id
*           z1~deposit_amount
*           z1~account_group
*           z1~is_lucky_retailer
*           k~erdat
*           z4~zguncel_limit
*      FROM zsog_fi_001_t_01 AS z1
*      INNER JOIN kna1 AS k ON k~kunnr = z1~retail_location_id
*      LEFT JOIN zsog_fi_012_t_02 AS z4 ON z4~kunnr = k~kunnr
*      INNER JOIN zsog_fi_001_t_02 AS z2 ON z2~retailer_debtor_id = z1~retail_location_id
*      INNER JOIN zsog_fi_001_t_03 AS z3 ON z3~districts_number = z1~sales_district
*      INTO TABLE et_retailer_info
*      WHERE k~kunnr IN lr_retailer_no
*        AND k~erdat IN lr_create_date.
**        AND k~kunnr LIKE 'SG%'.

*{   ->>> Added by Prodea Anıl YILDIRIM - 08.10.2019 16:01:16
*    SELECT z1~retail_location_id
*           z1~retailer_sub_object
*           z1~action_code
*           z1~business_name
*           z1~contact_name
*           z1~type
*           z1~classification
*           z1~street
*           z1~house_number
*           z1~zip_code
*           k~regio
*           z1~sales_district
*           z1~state
*           z1~delivery_street
*           z1~delivery_house_number
*           z1~delivery_zip_code
*           z1~delivery_city
*           z3~districts
*           z1~delivery_state
*           z1~phone1
*           z1~phone2
*           z1~mobile_phone
*           z1~email
*           z1~fax
*           z1~turkish_national_id
*           z1~tax_house
*           z2~tax_number
*           z1~supervisor
*           z1~sales_territory_region
*           z1~commission_package_id
*           z1~deposit_amount
*           z1~account_group
*           z1~is_lucky_retailer
*           k~erdat
*           z4~zguncel_limit
*      FROM zsog_fi_001_t_01 AS z1
*      INNER JOIN kna1  AS k ON k~kunnr = z1~retail_location_id
*      LEFT JOIN zsog_fi_012_t_02 AS z4 ON z4~kunnr = k~kunnr
*      INNER JOIN cdhdr AS c ON c~objectid = z1~retail_location_id
*      INNER JOIN zsog_fi_001_t_02 AS z2 ON z2~retailer_debtor_id = z1~retail_location_id
*      INNER JOIN zsog_fi_001_t_03 AS z3 ON z3~districts_number = z1~sales_district
*      APPENDING TABLE et_retailer_info
*      WHERE c~objectid   IN lr_retailer_no
*        AND c~udate      IN lr_create_date
*        AND c~objectclas EQ lv_objectclas
*        AND c~tcode      EQ lv_tcode
*        AND c~username   EQ lv_username.
*
*    DELETE ADJACENT DUPLICATES FROM et_retailer_info.
*}    <<<- End of  Added - 08.10.2019 16:01:16
*}     <<<- End of  Commented - 19.11.2019 11:16:54

*{   ->>> Added by Prodea Ozan Şahin - 19.11.2019 11:17:58

    CLEAR et_retailer_info[].

    SELECT z1~retail_location_id
           z1~retailer_sub_object
           z1~action_code
           z1~business_name
           z1~contact_name
           z1~type
           z1~classification
           z1~street
           z1~house_number
           z1~zip_code
           z3~city_code
           z1~sales_district
           z1~state
           z1~delivery_street
           z1~delivery_house_number
           z1~delivery_zip_code
           z1~delivery_city
           z3~districts
           z1~delivery_state
           z1~phone1
           z1~phone2
           z1~mobile_phone
           z1~email
           z1~fax
           z1~turkish_national_id
           z1~tax_house
           z2~tax_number
           z1~supervisor
           z1~sales_territory_region
           z1~commission_package_id
           z1~deposit_amount
           z1~account_group
           z1~is_lucky_retailer
           kb~erdat
           z4~zguncel_limit
      FROM knb1 AS kb
      INNER JOIN zsog_fi_001_t_01 AS z1 ON kb~kunnr = z1~retail_location_id
      LEFT JOIN  zsog_fi_012_t_02 AS z4 ON z4~kunnr = kb~kunnr
      INNER JOIN zsog_fi_001_t_02 AS z2 ON z2~retailer_debtor_id = z1~retail_location_id
      INNER JOIN zsog_fi_001_t_03 AS z3 ON z3~districts_number = z1~sales_district
      INTO TABLE et_retailer_info
      WHERE kb~kunnr IN lr_retailer_no
        AND kb~erdat IN lr_create_date
        AND kb~kunnr LIKE lv_sg
        AND kb~bukrs EQ lv_kunnr.

    IF i_create_date IS NOT INITIAL.

      SELECT z1~retail_location_id
             z1~retailer_sub_object
             z1~action_code
             z1~business_name
             z1~contact_name
             z1~type
             z1~classification
             z1~street
             z1~house_number
             z1~zip_code
             z3~city_code
             z1~sales_district
             z1~state
             z1~delivery_street
             z1~delivery_house_number
             z1~delivery_zip_code
             z1~delivery_city
             z3~districts
             z1~delivery_state
             z1~phone1
             z1~phone2
             z1~mobile_phone
             z1~email
             z1~fax
             z1~turkish_national_id
             z1~tax_house
             z2~tax_number
             z1~supervisor
             z1~sales_territory_region
             z1~commission_package_id
             z1~deposit_amount
             z1~account_group
             z1~is_lucky_retailer
             kb~erdat
             z4~zguncel_limit
        FROM knb1 AS kb
        INNER JOIN zsog_fi_013_t_01 AS t13 ON kb~kunnr = t13~kunnr
        INNER JOIN zsog_fi_001_t_01 AS z1  ON kb~kunnr = z1~retail_location_id
        LEFT OUTER JOIN  zsog_fi_012_t_02  AS z4 ON z4~kunnr = kb~kunnr
        INNER JOIN zsog_fi_001_t_02 AS z2  ON z2~retailer_debtor_id = z1~retail_location_id
        INNER JOIN zsog_fi_001_t_03 AS z3  ON z3~districts_number = z1~sales_district
        APPENDING TABLE et_retailer_info
        WHERE kb~kunnr  IN lr_retailer_no
          AND t13~datum IN lr_create_date
          AND t13~kunnr LIKE lv_sg
          AND kb~bukrs  EQ lv_kunnr.

      SORT et_retailer_info BY retail_location_id.
      DELETE ADJACENT DUPLICATES FROM  et_retailer_info COMPARING retail_location_id.
    ENDIF.
*}     <<<- End of  Added - 19.11.2019 11:17:58

    IF et_retailer_info IS INITIAL.
      MESSAGE e251(zfi) INTO lv_dummy.
      PERFORM get_message_text CHANGING ls_return-message.
      ls_return-type   = 'E'.
      ls_return-id     = 'ZFI'.
      ls_return-number = '251'.
      APPEND ls_return TO et_return[].
    ENDIF.
  ENDIF.

ENDFUNCTION.
