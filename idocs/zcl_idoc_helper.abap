*----------------------------------------------------------------------*
*       CLASS ZCL_IDOC_HELPER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_IDOC_HELPER definition
  public
  final
  create public .

public section.

  types:
    EDISDEF_TT TYPE STANDARD TABLE OF edisdef WITH DEFAULT KEY .
  types:
    edidd_tt TYPE STANDARD TABLE OF edid4 WITH DEFAULT KEY .
  types:
    edsappl_tt TYPE STANDARD TABLE OF edsappl WITH DEFAULT KEY .
  types:
    idocsyn_tt TYPE STANDARD TABLE OF idocsyn WITH DEFAULT KEY .
  types:
    edimsg_tt TYPE STANDARD TABLE OF edimsg WITH DEFAULT KEY .

  data SEGMENT_DEFINITIONS type EDISDEF_TT read-only .
  constants C_HLEVEL_START type EDI_HLEVEL value '02'. "#EC NOTEXT
  constants C_IDOC_DIRECT_INBOUND type EDI_DIRECT value 2. "#EC NOTEXT
  constants C_IDOC_DIRECT_OUTBOUND type EDI_DIRECT value 1. "#EC NOTEXT
  data COMMUNICATION_CONTROL type EDIDC_TT read-only .
  data CONTROL_RECORD type EDIDC read-only .
  data CURR_HLEVEL type EDI_HLEVEL read-only .
  data IDOC_DATA type EDIDD_TT read-only .
  data IDOC_MESSAGES type EDIMSG_TT read-only .
  data SEGMENTS_DESCRIPTION type EDSAPPL_TT read-only .
  data SYNTAX_DESCRIPTION type IDOCSYN_TT read-only .

  methods CONSTRUCTOR
    importing
      !CONTROL_RECORD type EDIDC
    raising
      resumable(ZCX_IDOC_CONTROL_RECORD_EMPTY)
      resumable(ZCX_IDOC_DIRECTION_EMPTY) .
  methods CONTROL_RECORD_SET
    importing
      !CONTROL_RECORD type EDIDC .
  methods CURR_HLEVEL_CALCULATE .
  methods DOWNLOAD_TO_LOCAL
    importing
      !DIRECTORY type STRING default ''
      !FILENAME type STRING default ''
      !RUN_NOTEPAD_IND type FLAG default ' '
    raising
      ZCX_CANNOT_RUN_IN_BACKGROUND
      ZCX_DIRECTORY_NOT_FOUND
      ZCX_DIRECTORY_EMPTY .
  methods IDOC_READ
    importing
      !DOCNUM type EDI_DOCNUM
    raising
      resumable(ZCX_IDOC_NUMBER_EMPTY)
      resumable(ZCX_IDOC_NOT_FOUND) .
  methods METADATA_READ
    raising
      ZCX_IDOC_TYPE_NOT_FOUND
      ZCX_IDOC_TYPE_NOT_RELEASED
      ZCX_IDOC_TYPE_EMPTY .
  methods METADATA_READ_MESSAGES
    raising
      ZCX_NO_MESSAGES_FOR_IDOC_TYPE .
  methods METADATA_READ_SEGMENTS
    raising
      resumable(ZCX_IDOC_SYNTAX_MISSING)
      resumable(ZCX_IDOC_SEGMENTS_NOT_FOUND)
      resumable(ZCX_IDOC_SEGMENT_DEF_NOT_FOUND) .
  methods METADATA_READ_SYNTAX
    raising
      ZCX_IDOC_SYNTAX_MISSING .
  methods RECEIVE
    importing
      !DOCNUM type EDI_DOCNUM .
  methods RESTART .
  methods SEGMENT_ADD
    importing
      !SEGNAM type EDILSEGTYP
      !PSGNUM type EDI_PSGNUM default 0000
      !HLEVEL type EDI_HLEVEL default 00
      !DTINT2 type EDI_DTINT2 default 1000
      !SDATA type EDI_SDATA
    raising
      resumable(ZCX_IDOC_SEGMENT_NOT_FOUND) .
  methods SEND
    raising
      ZCX_IDOC_TYPE_EMPTY
      ZCX_IDOC_MESSAGE_TYPE_EMPTY
      ZCX_IDOC_RCV_PARTNR_TYP_EMPTY
      ZCX_IDOC_RCV_PRTNR_NO_EMPTY
      ZCX_IDOC_ERROR_IN_CONTROL
      ZCX_IDOC_ERROR_WRITING_STATUS
      resumable(ZCX_IDOC_ERROR_IN_DATA)
      resumable(ZCX_IDOC_SND_LOG_SYSTEM_UNKNO)
      resumable(ZCX_IDOC_OTHERS)
      resumable(ZCX_IDOC_NO_DATA) .
  methods STATUS_SET
    importing
      !STATUS type EDI_STATUS .
  methods STATUS_WRITE_TO_DB
    raising
      resumable(ZCX_IDOC_STATUS_EMPTY)
      resumable(ZCX_IDOC_FOREIGN_LOCK)
      resumable(ZCX_IDOC_NOT_FOUND)
      resumable(ZCX_IDOC_STATUS_RECORDS_EMPTY)
      resumable(ZCX_IDOC_STATUS_INVALID)
      resumable(ZCX_IDOC_DB_ERROR)
      resumable(ZCX_IDOC_OTHER) .
  methods TO_STRING_TABLE
    importing
      !TEST_IND type FLAG default 'X'
    returning
      value(STRING_TABLE) type STRING_TABLE
    raising
      ZCX_IDOC_DIRECTION_EMPTY
      ZCX_IDOC_RCV_PRTNR_NO_EMPTY
      ZCX_IDOC_RCV_PARTNR_TYP_EMPTY
      ZCX_IDOC_MESSAGE_TYPE_EMPTY
      ZCX_IDOC_TYPE_EMPTY
      ZCX_IDOC_CONTROL_RECORD_EMPTY
      ZCX_IDOC_SEGMENT_DEF_NOT_FOUND .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IDOC_HELPER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] CONTROL_RECORD                 TYPE        EDIDC
* | [!CX!] ZCX_IDOC_CONTROL_RECORD_EMPTY
* | [!CX!] ZCX_IDOC_DIRECTION_EMPTY
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.

  CLEAR:
    me->communication_control,
    me->control_record,
    me->curr_hlevel,
    me->idoc_data,
    me->segments_description,
    me->syntax_description.

  IF control_record IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_control_record_empty
      EXPORTING
        textid = zcx_idoc_control_record_empty=>zcx_idoc_control_record_empty.
  ENDIF.

  me->control_record_set( control_record ).

  IF NOT control_record-docnum IS INITIAL.
    me->idoc_read( control_record-docnum ).
  ELSE.
    me->curr_hlevel_calculate( ). " implicit in the idoc_read call
  ENDIF.

  IF me->control_record-direct IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_direction_empty
      EXPORTING
        textid = zcx_idoc_direction_empty=>zcx_idoc_direction_empty.
  ENDIF.

  IF NOT me->control_record-idoctp IS INITIAL.
    me->metadata_read( ).
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->CONTROL_RECORD_SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] CONTROL_RECORD                 TYPE        EDIDC
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONTROL_RECORD_SET.

  me->control_record = control_record.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->CURR_HLEVEL_CALCULATE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD curr_hlevel_calculate.

  DATA:
    lv_lines TYPE sytabix,
    ls_data LIKE LINE OF me->idoc_data.

  DESCRIBE TABLE me->idoc_data LINES lv_lines.
  IF lv_lines EQ 0.
    me->curr_hlevel = me->c_hlevel_start.
  ELSE.
    READ TABLE me->idoc_data INDEX lv_lines INTO ls_data.
    IF sy-subrc EQ 0.
      me->curr_hlevel = ls_data-hlevel.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->DOWNLOAD_TO_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] DIRECTORY                      TYPE        STRING (default ='')
* | [--->] FILENAME                       TYPE        STRING (default ='')
* | [--->] RUN_NOTEPAD_IND                TYPE        FLAG (default =' ')
* | [!CX!] ZCX_CANNOT_RUN_IN_BACKGROUND
* | [!CX!] ZCX_DIRECTORY_NOT_FOUND
* | [!CX!] ZCX_DIRECTORY_EMPTY
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD download_to_local.

  DATA:
    lv_file_separator TYPE c,
    lv_directory TYPE string,
    lv_filename TYPE string,
    lv_c_filename TYPE c LENGTH 128,
    lv_directory_exists_ind TYPE flag,
    lt_text TYPE string_table.

  IF NOT sy-batch IS INITIAL.
    RAISE EXCEPTION TYPE zcx_cannot_run_in_background.
  ENDIF.

  IF directory IS INITIAL.
    cl_gui_frontend_services=>directory_browse( CHANGING selected_folder = lv_directory ).
    IF lv_directory IS INITIAL.
      RAISE EXCEPTION TYPE zcx_directory_empty.
    ENDIF.
  ELSE.
    lv_directory = directory.
  ENDIF.

  lv_directory_exists_ind = cl_gui_frontend_services=>directory_exist( lv_directory ).
  IF lv_directory_exists_ind EQ abap_false.
    RAISE EXCEPTION TYPE zcx_directory_not_found.
  ENDIF.

  cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = lv_file_separator ).

  IF filename IS INITIAL.
    lv_filename = |{ lv_directory }{ lv_file_separator }{ me->control_record-idoctp }.{ me->control_record-docnum }.idoc|.
  ELSE.
    lv_filename = filename.
  ENDIF.

  lt_text = me->to_string_table( ).

  cl_gui_frontend_services=>gui_download( EXPORTING filename = lv_filename
                                          CHANGING data_tab = lt_text ).

  IF NOT run_notepad_ind IS INITIAL.
    lv_c_filename = lv_filename.
    CALL FUNCTION 'GUI_RUN'
      EXPORTING
        command   = 'NOTEPAD.EXE'
        parameter = lv_c_filename.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->IDOC_READ
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOCNUM                         TYPE        EDI_DOCNUM
* | [!CX!] ZCX_IDOC_NUMBER_EMPTY
* | [!CX!] ZCX_IDOC_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD idoc_read.

  IF docnum IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_number_empty.
  ELSE.

    SELECT SINGLE * FROM edidc INTO me->control_record
    WHERE docnum EQ docnum.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_idoc_not_found.
    ELSE.
      SELECT * FROM edid4 INTO TABLE me->idoc_data
      WHERE docnum EQ docnum.
    ENDIF.
  ENDIF.

  me->curr_hlevel_calculate( ). " update the current hierarchy level

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->METADATA_READ
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_IDOC_TYPE_NOT_FOUND
* | [!CX!] ZCX_IDOC_TYPE_NOT_RELEASED
* | [!CX!] ZCX_IDOC_TYPE_EMPTY
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD metadata_read.

  DATA:
    ls_edbas TYPE edbas.

  IF me->control_record-idoctp IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_type_empty.
  ENDIF.

  SELECT SINGLE * FROM edbas INTO ls_edbas WHERE idoctyp EQ me->control_record-idoctp.
  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_idoc_type_not_found.
  ENDIF.

*  IF ls_edbas-released EQ space. "For some reason, unreleased custom idoc types are not a problem it seems?
*    RAISE EXCEPTION TYPE zcx_idoc_type_not_released
*      EXPORTING
*        textid    = zcx_idoc_type_not_released=>zcx_idoc_type_not_released
*        idoc_type = me->control_record-idoctp.
*  ENDIF.

  me->metadata_read_syntax( ).
  me->metadata_read_segments( ).
  me->metadata_read_messages( ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->METADATA_READ_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_NO_MESSAGES_FOR_IDOC_TYPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD metadata_read_messages.

  IF NOT me->control_record-idoctp IS INITIAL.
    IF NOT me->control_record-mestyp IS INITIAL.

      SELECT * FROM edimsg INTO TABLE me->idoc_messages
      WHERE idoctyp EQ me->control_record-idoctp
        AND mestyp EQ me->control_record-mestyp.
    ELSE.
      SELECT * FROM edimsg INTO TABLE me->idoc_messages
      WHERE idoctyp EQ me->control_record-idoctp.
    ENDIF.

    IF me->idoc_messages[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_no_messages_for_idoc_type
        EXPORTING
          textid = zcx_no_messages_for_idoc_type=>zcx_no_messages_for_idoc_type.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->METADATA_READ_SEGMENTS
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_IDOC_SYNTAX_MISSING
* | [!CX!] ZCX_IDOC_SEGMENTS_NOT_FOUND
* | [!CX!] ZCX_IDOC_SEGMENT_DEF_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD metadata_read_segments.

  IF me->syntax_description[] IS INITIAL.
    me->metadata_read_syntax( ).
  ENDIF.

  IF me->syntax_description[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_syntax_missing.
  ENDIF.

  SELECT * FROM edsappl INTO TABLE me->segments_description
  FOR ALL ENTRIES IN me->syntax_description
  WHERE segtyp EQ me->syntax_description-segtyp.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_idoc_segments_not_found.
  ENDIF.

  SELECT * FROM edisdef INTO TABLE me->segment_definitions
  FOR ALL ENTRIES IN me->syntax_description
  WHERE segtyp EQ me->syntax_description-segtyp.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_idoc_segment_def_not_found.
  ELSE.
    SORT me->segment_definitions BY version segdef DESCENDING.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->METADATA_READ_SYNTAX
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_IDOC_SYNTAX_MISSING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD metadata_read_syntax.

  SELECT * FROM idocsyn INTO TABLE me->syntax_description
  WHERE idoctyp EQ me->control_record-idoctp.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_idoc_syntax_missing.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->RECEIVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOCNUM                         TYPE        EDI_DOCNUM
* +--------------------------------------------------------------------------------------</SIGNATURE>
method RECEIVE.




endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->RESTART
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD RESTART.

  DATA:
    ls_temp_ctrl_rec TYPE edidc.

  ls_temp_ctrl_rec = me->control_record.

  CLEAR:
    me->communication_control,
    me->control_record,
    me->idoc_data.

  me->control_record-idoctp = ls_temp_ctrl_rec-idoctp.
  me->control_record-mestyp = ls_temp_ctrl_rec-mestyp.
  me->control_record-rcvprt = ls_temp_ctrl_rec-rcvprt.
  me->control_record-rcvprn = ls_temp_ctrl_rec-rcvprn.
  me->curr_hlevel = c_hlevel_start.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->SEGMENT_ADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] SEGNAM                         TYPE        EDILSEGTYP
* | [--->] PSGNUM                         TYPE        EDI_PSGNUM (default =0000)
* | [--->] HLEVEL                         TYPE        EDI_HLEVEL (default =00)
* | [--->] DTINT2                         TYPE        EDI_DTINT2 (default =1000)
* | [--->] SDATA                          TYPE        EDI_SDATA
* | [!CX!] ZCX_IDOC_SEGMENT_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD segment_add.

  DATA:
    ls_segment LIKE LINE OF me->idoc_data.

  CLEAR ls_segment.

  ls_segment-docnum = me->control_record-docnum.

  DESCRIBE TABLE me->idoc_data LINES ls_segment-segnum.
  ADD 1 TO ls_segment-segnum.

  ls_segment-segnam = segnam.

  IF psgnum NE '0000'.
    READ TABLE me->idoc_data WITH KEY segnum = psgnum BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_idoc_segment_not_found.
    ENDIF.
  ENDIF.

  ls_segment-psgnum = psgnum.

  IF hlevel EQ '00'.
    ls_segment-hlevel = me->curr_hlevel.
  ELSE.
    ls_segment-hlevel = hlevel.
  ENDIF.

  ls_segment-dtint2 = dtint2.
  ls_segment-sdata = sdata.

  APPEND ls_segment TO me->idoc_data.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->SEND
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_IDOC_TYPE_EMPTY
* | [!CX!] ZCX_IDOC_MESSAGE_TYPE_EMPTY
* | [!CX!] ZCX_IDOC_RCV_PARTNR_TYP_EMPTY
* | [!CX!] ZCX_IDOC_RCV_PRTNR_NO_EMPTY
* | [!CX!] ZCX_IDOC_ERROR_IN_CONTROL
* | [!CX!] ZCX_IDOC_ERROR_WRITING_STATUS
* | [!CX!] ZCX_IDOC_ERROR_IN_DATA
* | [!CX!] ZCX_IDOC_SND_LOG_SYSTEM_UNKNO
* | [!CX!] ZCX_IDOC_OTHERS
* | [!CX!] ZCX_IDOC_NO_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD send.

  DATA:
    lt_edidd TYPE STANDARD TABLE OF edidd,
    ls_edidd TYPE edidd,
    ls_edid4 TYPE edid4.

  IF me->control_record-idoctp IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_type_empty.
  ENDIF.

  IF me->control_record-mestyp IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_message_type_empty.
  ENDIF.

  IF me->control_record-rcvprt IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_rcv_partnr_typ_empty.
  ENDIF.

  IF me->control_record-rcvprn IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_rcv_prtnr_no_empty.
  ENDIF.

  CLEAR: lt_edidd, ls_edidd.
  LOOP AT me->idoc_data INTO ls_edid4.
    MOVE-CORRESPONDING ls_edid4 TO ls_edidd.
    APPEND ls_edidd TO lt_edidd.
  ENDLOOP.

  IF lt_edidd[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_no_data.
  ENDIF.

  me->control_record-direct = c_idoc_direct_outbound.

  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
    EXPORTING
      master_idoc_control            = me->control_record
*     OBJ_TYPE                       = ''
*     CHNUM                          = ''
    TABLES
      communication_idoc_control     = me->communication_control
      master_idoc_data               = lt_edidd
    EXCEPTIONS
      error_in_idoc_control          = 1
      error_writing_idoc_status      = 2
      error_in_idoc_data             = 3
      sending_logical_system_unknown = 4
      OTHERS                         = 5.

  CASE sy-subrc.
    WHEN 1. RAISE EXCEPTION TYPE zcx_idoc_error_in_control.
    WHEN 2. RAISE EXCEPTION TYPE zcx_idoc_error_writing_status.
    WHEN 3. RAISE EXCEPTION TYPE zcx_idoc_error_in_data.
    WHEN 4. RAISE EXCEPTION TYPE zcx_idoc_snd_log_system_unkno.
    WHEN 5. RAISE EXCEPTION TYPE zcx_idoc_others.
  ENDCASE.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->STATUS_SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] STATUS                         TYPE        EDI_STATUS
* +--------------------------------------------------------------------------------------</SIGNATURE>
method STATUS_SET.

  me->control_record-status = status.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->STATUS_WRITE_TO_DB
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_IDOC_STATUS_EMPTY
* | [!CX!] ZCX_IDOC_FOREIGN_LOCK
* | [!CX!] ZCX_IDOC_NOT_FOUND
* | [!CX!] ZCX_IDOC_STATUS_RECORDS_EMPTY
* | [!CX!] ZCX_IDOC_STATUS_INVALID
* | [!CX!] ZCX_IDOC_DB_ERROR
* | [!CX!] ZCX_IDOC_OTHER
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD status_write_to_db.

  DATA:
    lt_status TYPE STANDARD TABLE OF bdidocstat,
    ls_status TYPE bdidocstat.

  IF me->control_record-status IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_status_empty.
  ENDIF.

  MOVE-CORRESPONDING: syst TO ls_status,
                      control_record TO ls_status.
  APPEND ls_status TO lt_status.

  CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
    EXPORTING
      idoc_number               = me->control_record-docnum
    TABLES
      idoc_status               = lt_status
    EXCEPTIONS
      idoc_foreign_lock         = 1
      idoc_not_found            = 2
      idoc_status_records_empty = 3
      idoc_status_invalid       = 4
      db_error                  = 5
      OTHERS                    = 6.

  CASE sy-subrc.
    WHEN 1. RAISE EXCEPTION TYPE zcx_idoc_foreign_lock.
    WHEN 2. RAISE EXCEPTION TYPE zcx_idoc_not_found.
    WHEN 3. RAISE EXCEPTION TYPE zcx_idoc_status_records_empty.
    WHEN 4. RAISE EXCEPTION TYPE zcx_idoc_status_invalid.
    WHEN 5. RAISE EXCEPTION TYPE zcx_idoc_db_error.
    WHEN 6. RAISE EXCEPTION TYPE zcx_idoc_other.
  ENDCASE.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IDOC_HELPER->TO_STRING_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] TEST_IND                       TYPE        FLAG (default ='X')
* | [<-()] STRING_TABLE                   TYPE        STRING_TABLE
* | [!CX!] ZCX_IDOC_DIRECTION_EMPTY
* | [!CX!] ZCX_IDOC_RCV_PRTNR_NO_EMPTY
* | [!CX!] ZCX_IDOC_RCV_PARTNR_TYP_EMPTY
* | [!CX!] ZCX_IDOC_MESSAGE_TYPE_EMPTY
* | [!CX!] ZCX_IDOC_TYPE_EMPTY
* | [!CX!] ZCX_IDOC_CONTROL_RECORD_EMPTY
* | [!CX!] ZCX_IDOC_SEGMENT_DEF_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD to_string_table.

  CONSTANTS:
    c_tabnam_edi_dc40 TYPE edi4tabnam VALUE 'EDI_DC40'.

  DATA:
    ls_edi_dc40 TYPE edi_dc40,
    ls_edi_dd40 TYPE edi_dd40,
    ls_edisdef TYPE edisdef,
    ls_edids TYPE edids,
    ls_edid4 TYPE edid4,
    lv_dtint2 TYPE c LENGTH 5,
    lv_line TYPE string.

  IF me->control_record IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_control_record_empty.
  ELSEIF me->control_record-direct IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_direction_empty.
  ELSEIF me->control_record-rcvprn IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_rcv_prtnr_no_empty.
  ELSEIF me->control_record-rcvprt IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_rcv_partnr_typ_empty.
  ELSEIF me->control_record-mestyp IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_message_type_empty.
  ELSEIF me->control_record-idoctp IS INITIAL.
    RAISE EXCEPTION TYPE zcx_idoc_type_empty.
  ENDIF.

  CLEAR string_table.

  MOVE-CORRESPONDING: sy TO ls_edi_dc40,
                      me->control_record TO ls_edi_dc40.

  ls_edi_dc40-tabnam = c_tabnam_edi_dc40.
  ls_edi_dc40-idoctyp = me->control_record-idoctp.
  ls_edi_dc40-test = test_ind.

  IF ls_edi_dc40-credat IS INITIAL.
    ls_edi_dc40-credat = sy-datum.
  ENDIF.

  IF ls_edi_dc40-cretim IS INITIAL.
    ls_edi_dc40-cretim = sy-uzeit.
  ENDIF.

  APPEND ls_edi_dc40 TO string_table.

  LOOP AT me->idoc_data INTO ls_edid4.

    MOVE-CORRESPONDING ls_edid4 TO ls_edi_dd40.

    READ TABLE me->segment_definitions WITH KEY segtyp = ls_edid4-segnam INTO ls_edisdef.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_idoc_segment_def_not_found.
    ELSE.
      ls_edi_dd40-segnam = ls_edisdef-segdef.
    ENDIF.

    APPEND ls_edi_dd40 TO string_table.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.