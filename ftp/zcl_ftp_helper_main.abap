class ZCL_FTP_HELPER definition
  public
  final
  create public .

*"* public components of class ZCL_FTP_HELPER
*"* do not include other source files here!!!
public section.

  data FTP_HANDLE type INT4 read-only .
  data FTP_HOST type CHAR64 read-only .
  data FTP_USERNAME type CHAR30 read-only .
  data RFC_DESTINATION type CHAR64 read-only value 'SAPFTPA'. "#EC NOTEXT .

  methods FTP_LS
    importing
      !RELATIVE_DIRECTORY type STRING optional
    exporting
      value(FILE_LISTING) type STRING_TABLE
      !FILE_COUNTER type INT4
      !ERROR_COUNTER type INT4 .
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
      !RFC_DESTINATION type RFCDEST default 'SAPFTPA' .
  methods FTP_DISCONNECT .
  methods FTP_SEND
    importing
      !DATA type STRING_TABLE .
  methods HTTP_SCRAMBLE
    importing
      !PASSWORD type STRING .

*"* dummy include to reduce generation dependencies between
*"* class ZCL_FTP_HELPER and it's users.
*"* touched if any type reference has been changed

*"* protected components of class ZCL_FTP_HELPER
*"* do not include other source files here!!!
protected section.

  data FTP_KEY type INT4 .
  data FTP_PASSWORD type CHAR30 .
  data FTP_SCRAMBLED_PASSWORD type CHAR30 .

*"* private components of class ZCL_FTP_HELPER
*"* do not include other source files here!!!
private section.

ENDCLASS.


CLASS ZCL_FTP_HELPER% IMPLEMENTATION.
method FTP_CONNECT.

  me->ftp_username = user.
  me->ftp_password = password.

  me->http_scramble( password ).

  me->ftp_host = host.
  me->rfc_destination = rfc_destination.

  CALL FUNCTION 'FTP_CONNECT'
    EXPORTING
      user                   = me->ftp_username
      password               = me->ftp_scrambled_password
      host                   = me->ftp_host
      rfc_destination        = me->rfc_destination
   IMPORTING
      HANDLE                 = me->ftp_handle
   EXCEPTIONS
     NOT_CONNECTED          = 1
     OTHERS                 = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

endmethod.

method FTP_DISCONNECT.
endmethod.

method FTP_SEND.
endmethod.

method HTTP_SCRAMBLE.

  DATA:
    lv_sourcelen TYPE i,
    lv_salt      TYPE i,
    lr_random    TYPE REF TO cl_abap_random_int.

  me->ftp_password = password.
  lr_random = cl_abap_random_int=>create( min = 1 max = 2147483647 ).
  lv_salt = sy-uzeit(2).
  DO lv_salt TIMES.
    me->ftp_key = lr_random->get_next( ).
  ENDDO.

  SET EXTENDED CHECK OFF.

  lv_sourcelen = STRLEN( password ).

  CALL FUNCTION 'HTTP_SCRAMBLE'
    EXPORTING
      SOURCE            = me->ftp_password
      sourcelen         = lv_sourcelen
      key               = me->ftp_key
   IMPORTING
      DESTINATION       = me->ftp_scrambled_password.

endmethod.

METHOD ftp_command.

  CALL FUNCTION 'FTP_COMMAND'
    EXPORTING
      handle                = me->ftp_handle
      command               = command
*     COMPRESS              =
      rfc_destination       = me->rfc_destination
*     VERIFY                =
*   IMPORTING
*     FILESIZE              =
*     FILEDATE              =
*     FILETIME              =
    TABLES
      data                  = return_table
    EXCEPTIONS
      TCPIP_ERROR           = 1
      COMMAND_ERROR         = 2
      DATA_ERROR            = 3
      OTHERS                = 4
            .
  rc = sy-subrc.

ENDMETHOD.

METHOD ftp_cd.

  DATA:
    lv_command TYPE string,
    lt_result  TYPE string_table,
    lv_rc      TYPE sysubrc.

  CONCATENATE 'CD' relative_directory INTO lv_command SEPARATED BY space.

  me->ftp_command( EXPORTING command = lv_command
                   IMPORTING return_table = lt_result
                             rc           = lv_rc ).

ENDMETHOD.

METHOD ftp_ls.

  DATA:
    lv_command TYPE string,
    lt_result  TYPE string_table,
    lv_rc      TYPE sysubrc.

  CLEAR: file_listing, error_counter, file_counter.

  " Move to specified directory (optional)
  IF NOT relative_directory EQ space.
    CONCATENATE 'CD' relative_directory INTO lv_command SEPARATED BY space.

    me->ftp_command( EXPORTING command = lv_command
                     IMPORTING return_table = lt_result
                               rc           = lv_rc ).

    IF NOT lv_rc EQ 0.
      ADD 1 TO error_counter.
    ENDIF.
  ENDIF.

  me->ftp_command( EXPORTING command = 'LS'
                   IMPORTING return_table = file_listing
                             rc           = lv_rc ).

  IF NOT lv_rc EQ 0.
    ADD 1 TO error_counter.
  ENDIF.

ENDMETHOD.

ENDCLASS.
