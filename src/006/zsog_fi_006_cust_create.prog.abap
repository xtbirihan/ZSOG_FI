*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_006_CUST_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsog_fi_006_cust_create.

INCLUDE: zsog_fi_006_cust_create_top,
         zsog_fi_006_cust_create_scr,
         zsog_fi_006_cust_create_f01.

INITIALIZATION.

*  DATA: lr_gos_manager TYPE REF TO cl_gos_manager,
*        ls_borident TYPE borident.
*
*  ls_borident-objtype = 'BUS2081'.
*  ls_borident-objkey = '12345678902011'.
*
*  CREATE OBJECT lr_gos_manager
*    EXPORTING
*      is_object      = ls_borident
*      ip_no_commit   = ''
*    EXCEPTIONS
*      object_invalid = 1.

START-OF-SELECTION.

  PERFORM upload_file TABLES gt_rmf USING p_rmf 'RMF'.
  PERFORM upload_file TABLES gt_rlf USING p_rlf 'RLF'.

  PERFORM create_tables.
  PERFORM initialize_alv.
  PERFORM display_alv.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rmf.
  PERFORM get_file CHANGING p_rmf.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rlf.
  PERFORM get_file CHANGING p_rlf.

INITIALIZATION.
  PERFORM initialize_program.

AT SELECTION-SCREEN.
  PERFORM download_sample_csv.
