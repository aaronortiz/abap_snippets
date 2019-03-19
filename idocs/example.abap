*&---------------------------------------------------------------------*
*& Report  ZIDOC_WRITE_TO_LOCAL_FILE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zidoc_write_to_local_file.

SELECTION-SCREEN BEGIN OF BLOCK idoc WITH FRAME.

PARAMETERS: p_docnum TYPE edi_docnum OBLIGATORY,
            p_open AS CHECKBOX DEFAULT space.

SELECTION-SCREEN END OF BLOCK idoc.

DATA:
  ls_edidc TYPE edidc,
  lr_idoc_helper type REF TO zcl_idoc_helper.

START-OF-SELECTION.

  clear ls_edidc.
  SELECT SINGLE docnum FROM edidc INTO ls_edidc-docnum WHERE docnum EQ p_docnum.

  IF sy-subrc NE 0.
    MESSAGE text-000 TYPE 'I'.
  ELSE.

    create OBJECT lr_idoc_helper type zcl_idoc_helper EXPORTING control_record = ls_edidc.
    lr_idoc_helper->download_to_local( run_notepad_ind = p_open ).

  ENDIF.s