class ZCL_FTP_HELPER definition
  public
  final
  create public .

*"* public components of class ZCL_FTP_HELPER
*"* do not include other source files here!!!
public section.

  types:
    BEGIN OF ftp_data_s,
          line TYPE c LENGTH 100,
      END OF ftp_data_s .
  types:
    BEGIN OF ftp_long_text_s,
        line TYPE c LENGTH 1024,
      END OF ftp_long_text_s .
  types:
    ftp_data_t TYPE STANDARD TABLE OF ftp_data_s WITH DEFAULT KEY .
  types:
    ftp_long_text_t TYPE STANDARD TABLE OF ftp_long_text_s WITH DEFAULT KEY .

  data IS_IN_ASCII type FLAG read-only value 'X'. "#EC NOTEXT .
  data IS_IN_PASV type FLAG read-only value 'X'. "#EC NOTEXT .
  constants C_FTP_KEY type INT4 value 26101957. "#EC NOTEXT
  data FTP_HANDLE type INT4 read-only .
  data FTP_HOST type CHAR64 read-only .
  data FTP_USERNAME type CHAR30 read-only .
  data RFC_DESTINATION type RFCDEST read-only value 'SAPFTPA'. "#EC NOTEXT .

  methods FTP_ASCII
    returning
      value(RC) type SYSUBRC .
  methods FTP_BIN
    returning
      value(RC) type SYSUBRC .
  methods FTP_CD
    importing
      !RELATIVE_DIRECTORY type STRING
    returning
      value(RC) type SYSUBRC .
  methods FTP_COMMAND
    importing
      !COMMAND type STRING
    exporting
      value(RETURN_TABLE) type STRING_TABLE
      !RC type SYSUBRC .
  methods FTP_CONNECT
    importing
      !USER type STRING
      !PASSWORD type STRING
      !HOST type STRING
      !RFC_DESTINATION type RFCDEST default 'SAPFTPA'
      !START_IN_FOLDER type STRING default ''
      !START_IN_PASV type FLAG default 'X'
      !START_IN_ASCII type FLAG default 'X'
    returning
      value(RC) type SYSUBRC .
  methods FTP_DISCONNECT
    returning
      value(RC) type SYSUBRC .
  methods FTP_LS
    importing
      !RELATIVE_DIRECTORY type STRING optional
      !FILTER type STRING default '*'
    exporting
      value(FILE_LISTING) type STRING_TABLE
      !FILE_COUNTER type INT4
      !ERROR_COUNTER type INT4 .
  methods FTP_PASV_MODE
    importing
      !ON_OFF type STRING default 'on'
    returning
      value(RC) type SYSUBRC .
  methods FTP_RECV
    importing
      !REMOTE_PATH type STRING default ''
      !FILENAME type STRING
    exporting
      !DATA type STRING_TABLE
      value(RC) type SYSUBRC .
  methods FTP_SEND
    importing
      !REMOTE_PATH type STRING
      !FILENAME type STRING
      !DATA type STRING_TABLE
    returning
      value(RC) type SYSUBRC .
  methods HTTP_SCRAMBLE
    importing
      !PASSWORD type STRING
    returning
      value(RC) type SYSUBRC .

*"* dummy include to reduce generation dependencies between
*"* class ZCL_FTP_HELPER and it's users.
*"* touched if any type reference has been changed

*"* protected components of class ZCL_FTP_HELPER
*"* do not include other source files here!!!
protected section.

  data FTP_PASSWORD type CHAR30 .
  data FTP_SCRAMBLED_PASSWORD type CHAR30 .

*"* private components of class ZCL_FTP_HELPER
*"* do not include other source files here!!!
private section.

ENDCLASS.


CLASS ZCL_FTP_HELPER% IMPLEMENTATION.
METHOD ftp_connect.

  DATA:
    lr_param_missing_err TYPE REF TO cx_sy_dyn_call_param_missing.

  rc = 1.

  IF NOT user IS INITIAL AND NOT password IS INITIAL AND NOT host IS INITIAL AND NOT rfc_destination IS INITIAL.

    me->ftp_username = user.
    me->ftp_password = password.
    me->is_in_ascii  = start_in_ascii.
    me->is_in_pasv   = start_in_pasv.

    me->http_scramble( password ).

    me->ftp_host = host.
    me->rfc_destination = rfc_destination.

    TRY.

        CALL FUNCTION 'FTP_CONNECT'
          EXPORTING
            user            = me->ftp_username
            password        = me->ftp_scrambled_password
            host            = me->ftp_host
            rfc_destination = me->rfc_destination
          IMPORTING
            handle          = me->ftp_handle
          EXCEPTIONS
            not_connected   = 1
            OTHERS          = 2.
        rc = sy-subrc.

      CATCH cx_sy_dyn_call_param_missing INTO lr_param_missing_err.
    ENDTRY.

    IF rc = 0 AND NOT start_in_folder EQ space. " Go to starting folder
      rc = me->ftp_cd( start_in_folder ).
    ENDIF.
    IF rc = 0 AND me->is_in_pasv NE space. " Set pasv mode if requested
      rc = me->ftp_pasv_mode( ).
    ENDIF.
    IF rc = 0 AND me->is_in_ascii NE space. " Set ASCII or BIN mode
      rc = me->ftp_ascii( ).
    ELSE.
      rc = me->ftp_bin( ).
    ENDIF.

  ENDIF.

ENDMETHOD.

METHOD ftp_disconnect.

  rc = 0.

ENDMETHOD.

METHOD ftp_send.

  DATA:
    lv_filename TYPE char128,
    lt_data     TYPE ftp_long_text_t.

  rc = me->ftp_cd( remote_path ).
  lv_filename = filename.
  CLEAR lt_data.
  APPEND LINES OF data TO lt_data.

  IF rc EQ 0.

    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle         = me->ftp_handle
        fname          = lv_filename          "file path of destination system
        character_mode = 'X'
      TABLES
        text           = lt_data[]
      EXCEPTIONS
        tcpip_error    = 1
        command_error  = 2
        data_error     = 3
        OTHERS         = 4.

    rc = sy-subrc.

  ENDIF.

ENDMETHOD.

METHOD http_scramble.

  DATA:
    lv_sourcelen  TYPE i,
    lv_salt       TYPE i,
    lr_random     TYPE REF TO cl_abap_random_int,
    lr_random_err TYPE REF TO cx_abap_random.

  rc = 1.
  IF NOT password IS INITIAL.

    me->ftp_password = password.

    SET EXTENDED CHECK OFF.

    lv_sourcelen = STRLEN( password ).

    CALL FUNCTION 'HTTP_SCRAMBLE'
      EXPORTING
        SOURCE      = me->ftp_password
        sourcelen   = lv_sourcelen
        key         = me->c_ftp_key
      IMPORTING
        destination = me->ftp_scrambled_password.

    rc = sy-subrc.

  ENDIF.

ENDMETHOD.

METHOD ftp_command.

  DATA:
    lv_command      type c LENGTH 80,
    lt_return_table TYPE ftp_data_t.

  lv_command = command.

  CALL FUNCTION 'FTP_COMMAND'
    EXPORTING
      handle                = me->ftp_handle
      command               = lv_command
      rfc_destination       = me->rfc_destination
    TABLES
      data                  = lt_return_table[]
    EXCEPTIONS
      tcpip_error           = 1
      command_error         = 2
      data_error            = 3
      OTHERS                = 4
            .
  rc = sy-subrc.

  APPEND LINES OF lt_return_table TO return_table.

ENDMETHOD.

METHOD ftp_cd.

  DATA:
    lv_command     TYPE string,
    lt_result      TYPE string_table.

  rc = 1.

  CONCATENATE 'cd' relative_directory INTO lv_command SEPARATED BY space.

  me->ftp_command( EXPORTING command = lv_command
                   IMPORTING return_table = lt_result
                             rc           = rc ).

ENDMETHOD.

METHOD ftp_ls.

  DATA:
    lv_command TYPE string,
    lt_result  TYPE string_table,
    lv_rc      TYPE sysubrc,
    lv_lines   TYPE i.

  CLEAR: file_listing, error_counter, file_counter.

  " Move to specified directory (optional)
  IF NOT relative_directory EQ space.
    lv_rc = me->ftp_cd( relative_directory ).

    IF NOT lv_rc EQ 0.
      ADD 1 TO error_counter.
    ENDIF.
  ENDIF.

  IF lv_rc EQ 0.
    CONCATENATE 'ls' filter INTO lv_command SEPARATED BY space.
    me->ftp_command( EXPORTING command      = lv_command
                     IMPORTING return_table = file_listing
                               rc           = lv_rc ).
  ENDIF.

  IF NOT lv_rc EQ 0.
    ADD 1 TO error_counter.
  ELSE.
*    "remove extra lines.
    DESCRIBE TABLE file_listing LINES lv_lines.
    IF lv_lines GE 5.
      DELETE file_listing INDEX lv_lines.
      DO 4 TIMES.
        DELETE file_listing INDEX 1.
      ENDDO.
    ENDIF.
  ENDIF.

ENDMETHOD.

METHOD ftp_pasv_mode.

  DATA:
    lv_command     TYPE string,
    lt_result      TYPE string_table.

  rc = 1.
  CONCATENATE 'set passive' on_off INTO lv_command SEPARATED BY space.

  me->ftp_command( EXPORTING command = lv_command
                   IMPORTING return_table = lt_result
                             rc           = rc ).

  IF rc EQ 0.
    IF on_off EQ 'on' OR on_off = 'ON'.
      me->is_in_pasv = 'X'.
    ELSE.
      CLEAR me->is_in_pasv.
    ENDIF.
  ENDIF.

ENDMETHOD.

METHOD ftp_ascii.

  DATA:
    lv_command     TYPE string,
    lt_result      TYPE string_table.

  rc = 1.
  lv_command = 'ascii'.

  me->ftp_command( EXPORTING command = lv_command
                   IMPORTING return_table = lt_result
                             rc           = rc ).

  IF rc EQ 0.
    me->is_in_ascii = 'X'.
  ENDIF.

ENDMETHOD.

METHOD ftp_bin.

  DATA:
    lv_command     TYPE string,
    lt_result      TYPE string_table.

  rc = 1.
  lv_command = 'bin'.

  me->ftp_command( EXPORTING command = lv_command
                   IMPORTING return_table = lt_result
                             rc           = rc ).

  IF rc EQ 0.
    CLEAR me->is_in_ascii.
  ENDIF.

ENDMETHOD.

METHOD ftp_recv.

  DATA:
    lv_filename TYPE char128,
    lt_data     TYPE ftp_long_text_t.

  IF NOT remote_path EQ space.
    rc = me->ftp_cd( remote_path ).
  ENDIF.
  lv_filename = filename.
  CLEAR lt_data.

  IF rc EQ 0.

    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle         = me->ftp_handle
        fname          = lv_filename          "file path of destination system
        character_mode = 'X'
      TABLES
        text           = lt_data[]
      EXCEPTIONS
        tcpip_error    = 1
        command_error  = 2
        data_error     = 3
        OTHERS         = 4.

    rc = sy-subrc.

    IF rc EQ 0.
      APPEND LINES OF lt_data TO data.
    ENDIF.

  ENDIF.

ENDMETHOD.

ENDCLASS.
