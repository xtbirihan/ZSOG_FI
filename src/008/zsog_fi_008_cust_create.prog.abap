*&---------------------------------------------------------------------*
*& Report  ZSOG_FI_006_CUST_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsog_fi_008_cust_create.

INCLUDE ZSOG_FI_008_CUST_CREATE_TOP.
INCLUDE ZSOG_FI_008_CUST_CREATE_SCR.
INCLUDE ZSOG_FI_008_CUST_CREATE_F01.

INITIALIZATION.


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
